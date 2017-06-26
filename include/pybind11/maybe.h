/*
    pybind11/maybe.h: An optional-like container used by type casters

    Copyright (c) 2017 Wenzel Jakob <wenzel.jakob@epfl.ch>

    All rights reserved. Use of this source code is governed by a
    BSD-style license that can be found in the LICENSE file.
*/

#pragma once

#include "common.h"

NAMESPACE_BEGIN(pybind11)
NAMESPACE_BEGIN(detail)

struct in_place_t { };
constexpr in_place_t in_place{};

struct nothing_t { };

/// Defines: storage, default and in-place constructors, destructor
template <typename T, bool = std::is_trivially_destructible<T>::value>
struct maybe_storage;

template <typename T>
struct maybe_storage<T, /*is_trivially_destructible*/true> {
    union {
        char nothing;
        T something;
    };
    bool initialized = false;

    constexpr maybe_storage() noexcept : nothing() { }

    template <typename... Args>
    constexpr maybe_storage(in_place_t, Args &&...args)
        : something(std::forward<Args>(args)...), initialized(true) { }
};

template <typename T>
struct maybe_storage<T, /*is_trivially_destructible*/false> {
    union {
        char nothing;
        T something;
    };
    bool initialized = false;

    constexpr maybe_storage() noexcept : nothing() { }

    template <typename... Args>
    constexpr maybe_storage(in_place_t, Args &&...args)
        : something(std::forward<Args>(args)...), initialized(true) { }

    ~maybe_storage() { if (initialized) something.~T(); }
};

/// Defines: copy and move constructors
template <typename T, bool = std::is_trivially_copyable<T>::value>
struct maybe_base;

template <typename T>
struct maybe_base<T, /*is_trivially_copyable*/true> : maybe_storage<T> {
    using maybe_storage<T>::maybe_storage;
};

template <typename T>
struct maybe_base<T, /*is_trivially_copyable*/false> : maybe_storage<T> {
    using base = maybe_storage<T>;
    using base::base;
    using base::something;
    using base::initialized;

    constexpr maybe_base() = default;

    maybe_base(const maybe_base &other) {
        if (other.initialized) {
            ::new (&something) T(other.something);
            initialized = true;
        }
    }

    maybe_base(maybe_base &&other) noexcept(std::is_nothrow_move_constructible<T>::value) {
        if (other.initialized) {
            ::new (&something) T(std::move(other.something));
            initialized = true;
        }
    }
};

/// Usable maybe type, but without an extraction policy.
/// The API is similar to `std::optional` with some important differences:
///  * Only the functions that we need are defined
///  * Assignment operators are explicitly deleted: not needed and not efficient
///  * `value()` is not checked -- it does not throw if `!had_value()`
template <typename T>
class basic_maybe : private maybe_base<T> {
    using base = maybe_base<T>;
    using base::something;
    using base::initialized;

public:
    constexpr basic_maybe() = default;
    constexpr basic_maybe(nothing_t) noexcept { }

    basic_maybe(const basic_maybe &) = default;
    basic_maybe(basic_maybe &&) = default;

    basic_maybe &operator=(const basic_maybe &) = delete;
    basic_maybe &operator=(basic_maybe &&) = delete;

    template <class U = T, enable_if_t<std::is_constructible<T, U &&>::value, int> = 0>
    constexpr basic_maybe(U &&u) : base(in_place, std::forward<U>(u)) { }

    template <typename... Args, enable_if_t<std::is_constructible<T, Args...>::value, int> = 0>
    constexpr basic_maybe(in_place_t, Args &&...args)
        : base(in_place, std::forward<Args>(args)...) { }

    constexpr bool has_value() const noexcept { return initialized; }
    constexpr explicit operator bool() const noexcept { return initialized; }

    constexpr const T &value() const & noexcept { return something; }
    constexpr T &value() & noexcept { return something; }
    constexpr T &&value() && noexcept { return std::move(something); }

    constexpr const T &operator*() const & noexcept { return something; }
    constexpr T &operator*() & noexcept { return something; }
    constexpr T &&operator*() && noexcept { return std::move(something);}

    constexpr const T *operator->() const noexcept { return &something; }
    constexpr T *operator->() noexcept { return &something; }
};

/// A tag which indicates the target type for a `maybe` value extraction
template <typename T> struct to { };
template <typename T> struct to<const T> : to<T> { };
template <typename T> struct to<const T &> : to<T &> { };
template <typename T> struct to<const T *> : to<T *> { };
template <typename T> struct to<T *&> : to<T *> { };

/// Good default for types held by value (i.e. not exported with `py::class_`).
/// Both `to<T>` and `to<T &&>` targets return an rvalue reference for efficiently.
struct default_extraction_policy {
    template <typename V, typename T> static T  *apply(V &&x, to<T *>)  { return &x;           }
    template <typename V, typename T> static T  &apply(V &&x, to<T &>)  { return x;            }
    template <typename V, typename T> static T &&apply(V &&x, to<T>)    { return std::move(x); }
    template <typename V, typename T> static T &&apply(V &&x, to<T &&>) { return std::move(x); }
};

/// Final type, including an extraction policy
template <typename T, typename Policy = default_extraction_policy>
class maybe : public basic_maybe<T> {
public:
    using policy = Policy;
    using basic_maybe<T>::basic_maybe;

    constexpr maybe() = default;
};

/// The default policy for `py::class_` types. Here, `to<T>` is a copy and `to<T &&>`
/// is explicitly deleted to prevent accidentally moving out of a Python instance.
/// An instance can still be force-moved with `std::move(extract<T &>(maybe))`.
struct class_extraction_policy {
    template <typename T>
    static T *apply(void *p, to<T *>) { return static_cast<T *>(p); }

    template <typename T>
    static T &apply(void *p, to<T &>) {
        if (p)
            return *static_cast<T *>(p);
        throw reference_cast_error();
    }

    template <typename T>
    static const T &apply(void *p, to<T>) { return apply(p, to<T &>{}); }

    template <typename T>
    static T &&apply(void *p, to<T &&>) {
        static_assert(deferred_t<std::false_type, T>::value,
                      "Can't convert argument to an rvalue reference. "
                      "It's not safe to move out of a Python instance");
        return std::move(apply(p, to<T &>{})); // implemented to avoid compiler warnings
    }
};

/// `maybe` container for all `py::class_` types.
/// `void *` points to the C++ object inside a Python instance.
using maybe_class = maybe<void *, class_extraction_policy>;

/// Assumes `has_value() == true` and *unconditionally* casts `maybe` to an rvalue,
/// i.e. `extract<T>(maybe)` always behaves like `extract<T>(std::move(maybe))`.
template <typename T, typename Maybe, typename Policy = typename intrinsic_t<Maybe>::policy>
auto extract(Maybe &&maybe) -> decltype(Policy::apply(std::move(maybe).value(), to<T>{})) {
    return Policy::apply(std::move(maybe).value(), to<T>{});
}

NAMESPACE_END(detail)
NAMESPACE_END(pybind11)
