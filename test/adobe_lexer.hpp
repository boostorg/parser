/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */
#ifndef BOOST_PARSER_TEST_ADOBE_LEXER
#define BOOST_PARSER_TEST_ADOBE_LEXER

#include <boost/parser/parser.hpp>
#include <boost/parser/lexer.hpp>


namespace bp = boost::parser;

enum class adobe_tokens {
    keyword_true_false,
    keyword_empty,
    identifier,
    lead_comment,
    trail_comment,
    quoted_string,
    number,
    eq_op,
    rel_op,
    mul_op,
    define,
    or_,
    and_
};

constexpr auto true_false =
    bp::token_spec<"true|false", adobe_tokens::keyword_true_false, bool>;
constexpr auto empty = bp::token_spec<"empty", adobe_tokens::keyword_empty>;
constexpr auto identifier =
    bp::token_spec<"[a-zA-Z]\\w*", adobe_tokens::identifier>;
constexpr auto lead_comment = bp::token_spec<
    "\\/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*\\/",
    adobe_tokens::lead_comment>;
constexpr auto trail_comment =
    bp::token_spec<"\\/\\/.*$", adobe_tokens::trail_comment>;
constexpr auto quoted_string =
    bp::token_spec<"\\\"[^\\\"]*\\\"|'[^']*'", adobe_tokens::quoted_string>;
constexpr auto number =
    bp::token_spec<"\\d+(?:\\.\\d*)?", adobe_tokens::number, double>;
constexpr auto eq_op = bp::token_spec<"==|!=", adobe_tokens::eq_op>;
constexpr auto define = bp::token_spec<"<==", adobe_tokens::define>;
constexpr auto rel_op = bp::token_spec<"<|>|<=|>=", adobe_tokens::rel_op>;
constexpr auto mul_op = bp::token_spec<"\\*|\\/|%", adobe_tokens::mul_op>;
constexpr auto or_ = bp::token_spec<"\\|\\|", adobe_tokens::or_>;
constexpr auto and_ = bp::token_spec<"&&", adobe_tokens::and_>;

constexpr auto adobe_lexer = bp::lexer<char, adobe_tokens> | true_false |
                             empty | identifier | lead_comment | trail_comment |
                             quoted_string | number | eq_op | define | rel_op |
                             mul_op | or_ | and_ |
                             bp::token_chars<
                                 '=',
                                 '+',
                                 '-',
                                 '!',
                                 '?',
                                 ':',
                                 '.',
                                 ',',
                                 '(',
                                 ')',
                                 '[',
                                 ']',
                                 '{',
                                 '}',
                                 '@',
                                 ';'>;

#endif
