// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_DETAIL_MAKE_INPUT_SUBRANGE_HPP
#define BOOST_PARSER_DETAIL_MAKE_INPUT_SUBRANGE_HPP

#include <boost/parser/config.hpp>
#include <boost/parser/subrange.hpp>

#include <type_traits>


namespace boost::parser::detail {

    template<typename T>
    struct is_utf8_view : std::false_type
    {};
    template<typename V>
    struct is_utf8_view<text::utf8_view<V>> : std::true_type
    {};

#if BOOST_PARSER_USE_CONCEPTS

    template<typename T>
    using iterator_t = std::ranges::iterator_t<T>;
    template<typename T>
    using sentinel_t = std::ranges::sentinel_t<T>;
    template<typename T>
    using iter_value_t = std::iter_value_t<T>;
    template<typename T>
    using iter_reference_t = std::iter_reference_t<T>;
    template<typename T>
    using range_value_t = std::ranges::range_value_t<T>;
    template<typename T>
    using range_reference_t = std::ranges::range_reference_t<T>;
    template<typename T>
    using range_rvalue_reference_t = std::ranges::range_rvalue_reference_t<T>;

    template<typename T>
    constexpr bool is_parsable_code_unit_v = code_unit<T>;

#else

    template<typename T>
    using iterator_t =
        decltype(detail::text::detail::begin(std::declval<T &>()));
    template<typename Range>
    using sentinel_t =
        decltype(detail::text::detail::end(std::declval<Range &>()));
    template<typename T>
    using iter_value_t = typename std::iterator_traits<T>::value_type;
    template<typename T>
    using iter_reference_t = decltype(*std::declval<T &>());
    template<typename T>
    using iter_rvalue_reference_t = decltype(std::move(*std::declval<T &>()));
    template<typename T>
    using range_value_t = iter_value_t<iterator_t<T>>;
    template<typename T>
    using range_reference_t = iter_reference_t<iterator_t<T>>;
    template<typename T>
    using range_rvalue_reference_t = iter_rvalue_reference_t<iterator_t<T>>;

    template<typename T>
    constexpr bool is_parsable_code_unit_impl =
        std::is_same_v<T, char> || std::is_same_v<T, wchar_t> ||
#if defined(__cpp_char8_t)
        std::is_same_v<T, char8_t> ||
#endif
        std::is_same_v<T, char16_t> || std::is_same_v<T, char32_t>;

    template<typename T>
    constexpr bool is_parsable_code_unit_v =
        is_parsable_code_unit_impl<std::remove_cv_t<T>>;

#endif

    template<typename R>
    constexpr auto make_input_subrange(R && r) noexcept
    {
        using r_t = remove_cv_ref_t<R>;
        if constexpr (std::is_pointer_v<r_t>) {
            using value_type = iter_value_t<r_t>;
            if constexpr (std::is_same_v<value_type, char>) {
                return BOOST_PARSER_SUBRANGE(r, text::null_sentinel);
            } else {
                return r | text::as_utf32;
            }
        } else {
            using value_type = range_value_t<r_t>;
            if constexpr (text::detail::is_bounded_array_v<r_t>) {
                if constexpr (std::is_same_v<value_type, char>) {
                    auto first = detail::text::detail::begin(r);
                    auto last = detail::text::detail::end(r);
                    if (first != last && !*std::prev(last))
                        --last;
                    return BOOST_PARSER_SUBRANGE(first, last);
                } else {
                    return r | text::as_utf32;
                }
            } else {
                if constexpr (
                    std::is_same_v<value_type, char> &&
                    !is_utf8_view<r_t>::value) {
                    return BOOST_PARSER_SUBRANGE(
                        detail::text::detail::begin(r),
                        detail::text::detail::end(r));
                } else {
                    return r | text::as_utf32;
                }
            }
        }
    }

}

#endif
