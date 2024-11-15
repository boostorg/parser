// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#ifndef BOOST_PARSER_LEXER_FWD_HPP
#define BOOST_PARSER_LEXER_FWD_HPP

#include <ranges>
#include <vector>

namespace boost { namespace parser {

    /** TODO */
    template<
        std::ranges::contiguous_range V,
        typename Lexer,
        typename TokenCache = std::vector<typename Lexer::token_type>>
        requires std::ranges::view<V>
    struct tokens_view;

    namespace detail {
        template<typename T>
        constexpr bool is_tokens_view_v = false;
        template<typename V, typename Lexer, typename TokenCache>
        constexpr bool is_tokens_view_v<tokens_view<V, Lexer, TokenCache>> =
            true;
    }

}}

#endif
