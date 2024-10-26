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


int main()
{
#if 0
    std::string const str = "a";

    // attr out param, iter/sent
    {
        char out = 0;
        auto first = str.c_str();
        BOOST_TEST(prefix_parse(
            first, boost::parser::detail::text::null_sentinel, char_, out));
        first = str.c_str();
        BOOST_TEST(out == 'a');
        out = 0;
        first = str.c_str();
        BOOST_TEST(!prefix_parse(
            first,
            boost::parser::detail::text::null_sentinel,
            char_('b'),
            out));
        BOOST_TEST(out == 0);
    }
#endif

    return boost::report_errors();
}
