// Copyright (C) 2024 T. Zachary Laine
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
#define BOOST_PARSER_TESTING
#include <boost/parser/lexer.hpp>
#include <boost/parser/parser.hpp>

#include <boost/core/lightweight_test.hpp>


namespace bp = boost::parser;

bp::rule<class symbol_rule, std::string_view> const symrule = "symbols";
bp::symbols<std::string_view> rule_symbols;
auto const fwd_attr = [](auto & ctx) { _val(ctx) = _attr(ctx); };
auto symrule_def = rule_symbols[fwd_attr];
BOOST_PARSER_DEFINE_RULES(symrule);

constexpr auto I = bp::token_spec<"I", 0>;
constexpr auto V = bp::token_spec<"V", 1>;
constexpr auto X = bp::token_spec<"X", 2>;
constexpr auto L = bp::token_spec<"L", 3>;
constexpr auto C = bp::token_spec<"C", 4>;
constexpr auto arabic_num = bp::token_spec<"\\d+", 5, int>;

constexpr auto lexer = bp::lexer<char, int> | I | V | X | L | C | arabic_num;

int main()
{
// symbols_empty
{
    bp::symbols<int> roman_numerals;
    bp::symbols<std::string> named_strings;

    auto r = "I" | bp::to_tokens(lexer);
    BOOST_TEST(!bp::parse(r, roman_numerals));
    BOOST_TEST(!bp::parse(r, named_strings));
}

// symbols_simple
{
    bp::symbols<int> const roman_numerals = {
        {"I", 1}, {"V", 5}, {"X", 10}, {"L", 50}, {"C", 100}};
    bp::symbols<std::string> const named_strings = {
        {"I", "1"}, {"V", "5"}, {"X", "10"}, {"L", "50"}, {"C", "100"}};

    {
        auto const result =
            bp::parse("I" | bp::to_tokens(lexer), roman_numerals);
        BOOST_TEST(result);
        BOOST_TEST(*result == 1);
    }
    {
        auto const result =
            bp::parse("I" | bp::to_tokens(lexer), named_strings);
        BOOST_TEST(result);
        BOOST_TEST(*result == "1");
    }

    {
        auto const result =
            bp::parse("L" | bp::to_tokens(lexer), roman_numerals);
        BOOST_TEST(result);
        BOOST_TEST(*result == 50);
    }
    {
        auto const result =
            bp::parse("L" | bp::to_tokens(lexer), named_strings);
        BOOST_TEST(result);
        BOOST_TEST(*result == "50");
    }
}

// symbols_mutating
{
    bp::symbols<int> roman_numerals;
    roman_numerals.insert_for_next_parse("I", 1);
    roman_numerals.insert_for_next_parse("V", 5);
    roman_numerals.insert_for_next_parse("X", 10);
    auto const add_numeral = [&roman_numerals](auto & context) {
        using namespace boost::parser::literals;
        const std::string_view sv = bp::get(_attr(context), 0_c);
        roman_numerals.insert(context, sv, bp::get(_attr(context), 1_c));
    };
    auto const numerals_parser =
        ((I | V | X | L | C) >> arabic_num)[add_numeral] >> roman_numerals;

    {
        auto const result =
            bp::parse("L50L" | bp::to_tokens(lexer), numerals_parser);
        BOOST_TEST(result);
        BOOST_TEST(*result == 50);
        BOOST_TEST(!bp::parse("L", roman_numerals));
    }
    {
        auto const result =
            bp::parse("C100C" | bp::to_tokens(lexer), numerals_parser);
        BOOST_TEST(result);
        BOOST_TEST(*result == 100);
        BOOST_TEST(!bp::parse("C", roman_numerals));
    }
    {
        auto const result =
            bp::parse("L50C" | bp::to_tokens(lexer), numerals_parser);
        BOOST_TEST(!result);
    }
}

return boost::report_errors();
}
