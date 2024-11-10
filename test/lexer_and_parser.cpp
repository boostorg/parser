/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */

#include <boost/parser/lexer.hpp>
#include <boost/parser/parser.hpp>

#include <boost/core/lightweight_test.hpp>

#include "adobe_lexer.hpp"


namespace bp = boost::parser;

int main()
{
    // Minimal test; just instantiate the member functions, without involving
    // the parse() API.
    {
        bp::token<char> tokens[1] = {};
        auto p = bp::token_spec<"12", 12, int>;
        auto first = std::begin(tokens);
        auto const last = std::end(tokens);

        bp::detail::nope globals;
        bp::default_error_handler error_handler;

        // From parse_impl().
        bool success = true;
        int trace_indent = 0;
        bp::detail::symbol_table_tries_t symbol_table_tries;
        bp::detail::pending_symbol_table_operations_t
            pending_symbol_table_operations;
        bp::detail::scoped_apply_pending_symbol_table_operations apply_pending(
            pending_symbol_table_operations);
        auto context = bp::detail::make_context<false, false>(
            first,
            last,
            success,
            trace_indent,
            error_handler,
            globals,
            symbol_table_tries,
            pending_symbol_table_operations);
        auto const flags = bp::detail::flags::gen_attrs;

        std::optional<int> result =
            p(first, last, context, bp::ws, flags, success);
        (void)result;
    }

    // Minimal tests of building parsers from token_parser and token_spec.
    {
        auto parser1 = true_false(true);
        auto parser2 = true_false(false);
        (void)parser1;
        (void)parser2;
    }
    {
        auto parser = identifier("foo") >> '=' >> true_false >> ';';
        (void)parser;
    }

    // TODO    {
    // TODO        std::string str = "a";
    // TODO        BOOST_TEST(parse(str, char_));
    // TODO        BOOST_TEST(!parse(str, char_('b')));
    // TODO    }

    return boost::report_errors();
}
