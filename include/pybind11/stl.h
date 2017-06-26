/*
    pybind11/stl.h: Transparent conversion for STL data types

    Copyright (c) 2016 Wenzel Jakob <wenzel.jakob@epfl.ch>

    All rights reserved. Use of this source code is governed by a
    BSD-style license that can be found in the LICENSE file.
*/

#pragma once

#include "pybind11.h"
#include <set>
#include <unordered_set>
#include <map>
#include <unordered_map>
#include <iostream>
#include <list>
#include <valarray>

#if defined(_MSC_VER)
#pragma warning(push)
#pragma warning(disable: 4127) // warning C4127: Conditional expression is constant
#endif

#ifdef __has_include
// std::optional (but including it in c++14 mode isn't allowed)
#  if defined(PYBIND11_CPP17) && __has_include(<optional>)
#    include <optional>
#    define PYBIND11_HAS_OPTIONAL 1
#  endif
// std::experimental::optional (but not allowed in c++11 mode)
#  if defined(PYBIND11_CPP14) && __has_include(<experimental/optional>)
#    include <experimental/optional>
#    define PYBIND11_HAS_EXP_OPTIONAL 1
#  endif
// std::variant
#  if defined(PYBIND11_CPP17) && __has_include(<variant>)
#    include <variant>
#    define PYBIND11_HAS_VARIANT 1
#  endif
#elif defined(_MSC_VER) && defined(PYBIND11_CPP17)
#  include <optional>
#  include <variant>
#  define PYBIND11_HAS_OPTIONAL 1
#  define PYBIND11_HAS_VARIANT 1
#endif

NAMESPACE_BEGIN(pybind11)
NAMESPACE_BEGIN(detail)

template <typename Type, typename Key> struct set_caster {
    using type = Type;
    using key_conv = make_caster<Key>;

    static maybe<Type> try_load(handle src, bool convert) {
        if (!isinstance<pybind11::set>(src))
            return {};

        auto s = reinterpret_borrow<pybind11::set>(src);
        auto result = maybe<Type>(in_place);

        for (auto entry : s) {
            if (auto k = key_conv::try_load(entry, convert))
                result->insert(extract<Key>(k));
            else
                return {};
        }
        return result;
    }

    static handle cast(const type &src, return_value_policy policy, handle parent) {
        pybind11::set s;
        for (auto const &value: src) {
            auto value_ = reinterpret_steal<object>(key_conv::cast(value, policy, parent));
            if (!value_ || !s.add(value_))
                return handle();
        }
        return s.release();
    }

    PYBIND11_TYPE_CASTER2(type, _("Set[") + key_conv::name() + _("]"));
};

template <typename Type, typename Key, typename Value> struct map_caster {
    using key_conv   = make_caster<Key>;
    using value_conv = make_caster<Value>;

    static maybe<Type> try_load(handle src, bool convert) {
        if (!isinstance<dict>(src))
            return {};

        auto d = reinterpret_borrow<dict>(src);
        auto result = maybe<Type>(in_place);
        for (auto it : d) {
            auto k = key_conv::try_load(it.first.ptr(), convert);
            auto v = value_conv::try_load(it.second.ptr(), convert);
            if (!k || !v)
                return {};
            result->emplace(extract<Key>(k), extract<Value>(v));
        }
        return result;
    }

    static handle cast(const Type &src, return_value_policy policy, handle parent) {
        dict d;
        for (auto const &kv: src) {
            auto key = reinterpret_steal<object>(key_conv::cast(kv.first, policy, parent));
            auto value = reinterpret_steal<object>(value_conv::cast(kv.second, policy, parent));
            if (!key || !value)
                return handle();
            d[key] = value;
        }
        return d.release();
    }

    PYBIND11_TYPE_CASTER2(Type, _("Dict[") + key_conv::name() + _(", ") + value_conv::name() + _("]"));
};

template <typename Type, typename Value> struct list_caster {
    using value_conv = make_caster<Value>;

    static maybe<Type> try_load(handle src, bool convert) {
        if (!isinstance<sequence>(src))
            return {};

        auto s = reinterpret_borrow<sequence>(src);
        auto result = maybe<Type>(in_place);
        reserve_maybe(s, &*result);

        for (auto it : s) {
            if (auto element = value_conv::try_load(it, convert))
                result->push_back(extract<Value>(element));
            else
                return {};
        }
        return result;
    }

private:
    template <typename T = Type,
              enable_if_t<std::is_same<decltype(std::declval<T>().reserve(0)), void>::value, int> = 0>
    static void reserve_maybe(sequence s, Type *x) { x->reserve(s.size()); }
    static void reserve_maybe(sequence, void *) { }

public:
    static handle cast(const Type &src, return_value_policy policy, handle parent) {
        list l(src.size());
        size_t index = 0;
        for (auto const &value: src) {
            auto value_ = reinterpret_steal<object>(value_conv::cast(value, policy, parent));
            if (!value_)
                return handle();
            PyList_SET_ITEM(l.ptr(), (ssize_t) index++, value_.release().ptr()); // steals a reference
        }
        return l.release();
    }

    PYBIND11_TYPE_CASTER2(Type, _("List[") + value_conv::name() + _("]"));
};

template <typename Type, typename Alloc> struct type_caster<std::vector<Type, Alloc>>
 : list_caster<std::vector<Type, Alloc>, Type> { };

template <typename Type, typename Alloc> struct type_caster<std::list<Type, Alloc>>
 : list_caster<std::list<Type, Alloc>, Type> { };

template <typename ArrayType, typename Value, bool Resizable, size_t Size = 0> struct array_caster {
    using value_conv = make_caster<Value>;

private:
    template <bool R = Resizable>
    static bool require_size(ArrayType &array, enable_if_t<R, size_t> size) {
        if (array.size() != size)
            array.resize(size);
        return true;
    }
    template <bool R = Resizable>
    static bool require_size(ArrayType &, enable_if_t<!R, size_t> size) {
        return size == Size;
    }

public:
    static maybe<ArrayType> try_load(handle src, bool convert) {
        if (!isinstance<list>(src))
            return {};

        auto l = reinterpret_borrow<list>(src);
        auto result = maybe<ArrayType>(in_place);
        if (!require_size(*result, l.size()))
            return {};

        size_t ctr = 0;
        for (auto it : l) {
            if (auto element = value_conv::try_load(it, convert))
                (*result)[ctr++] = extract<Value>(element);
            else
                return {};
        }
        return result;
    }

    static handle cast(const ArrayType &src, return_value_policy policy, handle parent) {
        list l(src.size());
        size_t index = 0;
        for (auto const &value: src) {
            auto value_ = reinterpret_steal<object>(value_conv::cast(value, policy, parent));
            if (!value_)
                return handle();
            PyList_SET_ITEM(l.ptr(), (ssize_t) index++, value_.release().ptr()); // steals a reference
        }
        return l.release();
    }

    PYBIND11_TYPE_CASTER2(ArrayType, _("List[") + value_conv::name() + _<Resizable>(_(""), _("[") + _<Size>() + _("]")) + _("]"));
};

template <typename Type, size_t Size> struct type_caster<std::array<Type, Size>>
 : array_caster<std::array<Type, Size>, Type, false, Size> { };

template <typename Type> struct type_caster<std::valarray<Type>>
 : array_caster<std::valarray<Type>, Type, true> { };

template <typename Key, typename Compare, typename Alloc> struct type_caster<std::set<Key, Compare, Alloc>>
  : set_caster<std::set<Key, Compare, Alloc>, Key> { };

template <typename Key, typename Hash, typename Equal, typename Alloc> struct type_caster<std::unordered_set<Key, Hash, Equal, Alloc>>
  : set_caster<std::unordered_set<Key, Hash, Equal, Alloc>, Key> { };

template <typename Key, typename Value, typename Compare, typename Alloc> struct type_caster<std::map<Key, Value, Compare, Alloc>>
  : map_caster<std::map<Key, Value, Compare, Alloc>, Key, Value> { };

template <typename Key, typename Value, typename Hash, typename Equal, typename Alloc> struct type_caster<std::unordered_map<Key, Value, Hash, Equal, Alloc>>
  : map_caster<std::unordered_map<Key, Value, Hash, Equal, Alloc>, Key, Value> { };

// This type caster is intended to be used for std::optional and std::experimental::optional
template<typename Optional> struct optional_caster {
    using value_type = typename Optional::value_type;
    using value_caster = make_caster<value_type>;

    static handle cast(const Optional& src, return_value_policy policy, handle parent) {
        if (!src)
            return none().inc_ref();
        return value_caster::cast(*src, policy, parent);
    }

    static maybe<Optional> try_load(handle src, bool convert) {
        if (!src) {
            return {};
        } else if (src.is_none()) {
            return Optional{};  // default-constructed optional is empty
        }

        if (auto result = value_caster::try_load(src, convert)) {
            return extract<value_type>(result);
        }

        return {};
    }

    PYBIND11_TYPE_CASTER2(Optional, _("Optional[") + value_caster::name() + _("]"));
};

#if PYBIND11_HAS_OPTIONAL
template<typename T> struct type_caster<std::optional<T>>
    : public optional_caster<std::optional<T>> {};

template<> struct type_caster<std::nullopt_t>
    : public void_caster<std::nullopt_t> {};
#endif

#if PYBIND11_HAS_EXP_OPTIONAL
template<typename T> struct type_caster<std::experimental::optional<T>>
    : public optional_caster<std::experimental::optional<T>> {};

template<> struct type_caster<std::experimental::nullopt_t>
    : public void_caster<std::experimental::nullopt_t> {};
#endif

/// Visit a variant and cast any found type to Python
struct variant_caster_visitor {
    return_value_policy policy;
    handle parent;

    template <typename T>
    handle operator()(T &&src) const {
        return make_caster<T>::cast(std::forward<T>(src), policy, parent);
    }
};

/// Helper class which abstracts away variant's `visit` function. `std::variant` and similar
/// `namespace::variant` types which provide a `namespace::visit()` function are handled here
/// automatically using argument-dependent lookup. Users can provide specializations for other
/// variant-like classes, e.g. `boost::variant` and `boost::apply_visitor`.
template <template<typename...> class Variant>
struct visit_helper {
    template <typename... Args>
    static auto call(Args &&...args) -> decltype(visit(std::forward<Args>(args)...)) {
        return visit(std::forward<Args>(args)...);
    }
};

/// Generic variant caster
template <typename Variant> struct variant_caster;

template <template<typename...> class V, typename... Ts>
struct variant_caster<V<Ts...>> {
    static_assert(sizeof...(Ts) > 0, "Variant must consist of at least one alternative.");
    using Type = V<Ts...>;

    template <typename U, typename... Us>
    static maybe<Type> load_alternative(handle src, bool convert, type_list<U, Us...>) {
        if (auto result = make_caster<U>::try_load(src, convert))
            return extract<U>(result);
        return load_alternative(src, convert, type_list<Us...>{});
    }

    static maybe<Type> load_alternative(handle, bool, type_list<>) { return {}; }

    static maybe<Type> try_load(handle src, bool convert) {
        // Do a first pass without conversions to improve constructor resolution.
        // E.g. `py::int_(1).cast<variant<double, int>>()` needs to fill the `int`
        // slot of the variant. Without two-pass loading `double` would be filled
        // because it appears first and a conversion is possible.
        if (convert) {
            if (auto result = load_alternative(src, false, type_list<Ts...>{}))
                return result;
        }
        return load_alternative(src, convert, type_list<Ts...>{});
    }

    template <typename Variant>
    static handle cast(Variant &&src, return_value_policy policy, handle parent) {
        return visit_helper<V>::call(variant_caster_visitor{policy, parent},
                                     std::forward<Variant>(src));
    }

    PYBIND11_TYPE_CASTER2(Type, _("Union[") + detail::concat(make_caster<Ts>::name()...) + _("]"));
};

#if PYBIND11_HAS_VARIANT
template <typename... Ts>
struct type_caster<std::variant<Ts...>> : variant_caster<std::variant<Ts...>> { };
#endif
NAMESPACE_END(detail)

inline std::ostream &operator<<(std::ostream &os, const handle &obj) {
    os << (std::string) str(obj);
    return os;
}

NAMESPACE_END(pybind11)

#if defined(_MSC_VER)
#pragma warning(pop)
#endif
