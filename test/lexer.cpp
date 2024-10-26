/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */
#include <boost/parser/lexer.hpp>

#include "ill_formed.hpp"

#include <boost/core/lightweight_test.hpp>


namespace bp = boost::parser;

enum class my_tokens { foo, bar };

int main()
{
#if BOOST_PARSER_USE_CONCEPTS

    // formation of token_specs
    {
        auto const token_spec = bp::token_spec<"foo">(12);

        const bp::detail::token_spec<"foo", int, bp::none> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(my_tokens::foo);

        const bp::detail::token_spec<"foo", my_tokens, bp::none>
            token_spec_explicit(my_tokens::foo);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"bar">(my_tokens::bar);

        const bp::detail::token_spec<"bar", my_tokens, bp::none>
            token_spec_explicit(my_tokens::bar);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, 42);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12, 42);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, 'c');

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12, 'c');
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, 3u);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12, 3u);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, short(1));

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12, short(1));
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, 0.0f);

        const bp::detail::token_spec<"foo", int, double> token_spec_explicit(
            12, 0.0f);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo">(12, 1.0);

        const bp::detail::token_spec<"foo", int, double> token_spec_explicit(
            12, 1.0);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }

#endif

    return boost::report_errors();
}
