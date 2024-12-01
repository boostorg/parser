/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */

#define BOOST_PARSER_TESTING
#include <boost/parser/lexer.hpp>
#include <boost/parser/parser.hpp>

#include <boost/core/lightweight_test.hpp>


namespace bp = boost::parser;

constexpr auto true_false = bp::token_spec<"true|false", 0, bool>;
constexpr auto identifier = bp::token_spec<"[a-zA-Z]\\w*", 1>;

struct tf_tag
{};
struct id_tag
{};
constexpr bp::callback_rule<tf_tag, bool> callback_true_false = "";
constexpr bp::callback_rule<id_tag, std::string_view> callback_identifier = "";
constexpr auto callback_true_false_def = true_false;
constexpr auto callback_identifier_def = identifier;
BOOST_PARSER_DEFINE_RULES(callback_true_false, callback_identifier);

struct callbacks
{
    void operator()(id_tag, std::string_view sv) const { sv_ = sv; }
    void operator()(tf_tag, bool b) const { b_ = b; }
    std::string_view & sv_;
    bool & b_;
};

int main()
{
    auto assign_bool_parser = identifier >> '=' >> true_false >> ';';
    auto assign_bool_no_semi_parser = identifier >> '=' >> true_false;

    constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                           bp::token_chars<'=', ';'>;
    auto r = "foo = false;" | bp::to_tokens(lexer);

    // prefix_parse() w/attr
    {
        auto f = r.begin();
        auto const l = r.end();
        std::tuple<std::string_view, bool> result;
        auto success = bp::prefix_parse(f, l, assign_bool_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == "foo");
        BOOST_TEST(std::get<1>(result) == false);
    }
    {
        auto f = r.begin();
        auto const l = r.end();
        std::tuple<std::string_view, bool> result;
        auto success = bp::prefix_parse(f, l, assign_bool_no_semi_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == "foo");
        BOOST_TEST(std::get<1>(result) == false);
        BOOST_TEST(f != l);
    }

    // parse() w/attr
    {
        std::tuple<std::string_view, bool> result;
        auto success = bp::parse(r, assign_bool_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == "foo");
        BOOST_TEST(std::get<1>(result) == false);
    }
    {
        constexpr auto lexer = bp::lexer<char8_t, int> | true_false |
                               identifier | bp::token_chars<'=', ';'>;
        auto r8 = u8"foo = false;" | bp::to_tokens(lexer);

        std::tuple<std::u8string_view, bool> result;
        auto success = bp::parse(r8, assign_bool_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == u8"foo");
        BOOST_TEST(std::get<1>(result) == false);
    }
    {
        constexpr auto lexer = bp::lexer<char16_t, int> | true_false |
                               identifier | bp::token_chars<'=', ';'>;
        auto r16 = u"foo = false;" | bp::to_tokens(lexer);

        std::tuple<std::u16string_view, bool> result;
        auto success = bp::parse(r16, assign_bool_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == u"foo");
        BOOST_TEST(std::get<1>(result) == false);
    }
    {
        constexpr auto lexer = bp::lexer<char32_t, int> | true_false |
                               identifier | bp::token_chars<'=', ';'>;
        auto r32 = U"foo = false;" | bp::to_tokens(lexer);

        std::tuple<std::u32string_view, bool> result;
        auto success = bp::parse(r32, assign_bool_parser, result);
        BOOST_TEST(success);
        BOOST_TEST(std::get<0>(result) == U"foo");
        BOOST_TEST(std::get<1>(result) == false);
    }

    // prefix_parse() no attr
    {
        auto f = r.begin();
        auto const l = r.end();
        auto result = bp::prefix_parse(f, l, assign_bool_parser);
        BOOST_TEST(result);
        BOOST_TEST(std::get<0>(*result) == "foo");
        BOOST_TEST(std::get<1>(*result) == false);
    }
    {
        auto f = r.begin();
        auto const l = r.end();
        auto result = bp::prefix_parse(f, l, assign_bool_no_semi_parser);
        BOOST_TEST(result);
        BOOST_TEST(std::get<0>(*result) == "foo");
        BOOST_TEST(std::get<1>(*result) == false);
        BOOST_TEST(f != l);
    }

    // parse() no attr
    {
        auto result = bp::parse(r, assign_bool_parser);
        BOOST_TEST(result);
        BOOST_TEST(std::get<0>(*result) == "foo");
        BOOST_TEST(std::get<1>(*result) == false);
    }

    // callback_prefix_parse()
    {
        auto assign_bool_parser =
            callback_identifier >> '=' >> callback_true_false >> ';';

        auto f = r.begin();
        auto const l = r.end();
        std::string_view sv;
        bool b = false;
        auto success = bp::callback_prefix_parse(
            f, l, assign_bool_parser, callbacks{sv, b});
        BOOST_TEST(success);
        BOOST_TEST(sv == "foo");
        BOOST_TEST(b == false);
    }
    {
        auto assign_bool_no_semi_parser =
            callback_identifier >> '=' >> callback_true_false;

        auto f = r.begin();
        auto const l = r.end();
        std::string_view sv;
        bool b = false;
        auto success = bp::callback_prefix_parse(
            f, l, assign_bool_no_semi_parser, callbacks{sv, b});
        BOOST_TEST(success);
        BOOST_TEST(sv == "foo");
        BOOST_TEST(b == false);
        BOOST_TEST(f != l);
    }

    // callback_parse()
    {
        auto assign_bool_parser =
            callback_identifier >> '=' >> callback_true_false >> ';';

        std::string_view sv;
        bool b = false;
        auto success =
            bp::callback_parse(r, assign_bool_parser, callbacks{sv, b});
        BOOST_TEST(success);
        BOOST_TEST(sv == "foo");
        BOOST_TEST(b == false);
    }

    return boost::report_errors();
}
