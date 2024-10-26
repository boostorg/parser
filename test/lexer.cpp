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
        auto const token_spec = bp::token_spec<"foo", int>(12);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", char>(12);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", unsigned int>(12);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", short>(12);

        const bp::detail::token_spec<"foo", int, long long> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", float>(12);

        const bp::detail::token_spec<"foo", int, double> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }
    {
        auto const token_spec = bp::token_spec<"foo", double>(12);

        const bp::detail::token_spec<"foo", int, double> token_spec_explicit(
            12);
        static_assert(
            std::same_as<decltype(token_spec), decltype(token_spec_explicit)>);
    }

#endif

    return boost::report_errors();
}
