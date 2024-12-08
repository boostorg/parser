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

constexpr auto true_false = bp::token_spec<"true|false", 0, bool>;
constexpr auto identifier = bp::token_spec<"\\p{L}+", 1>;

int main()
{
    // token_parser
    {
        {
            constexpr auto lexer = bp::lexer<char, int> | true_false |
                                   identifier |
                                   bp::token_chars<'=', ';', '#', '$', '%'>;
            {
                constexpr auto parser = true_false;
                BOOST_TEST(bp::parse("true" | bp::to_tokens(lexer), parser));
                BOOST_TEST(bp::parse("false" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
            {
                constexpr auto parser = true_false(false);
                BOOST_TEST(!bp::parse("true" | bp::to_tokens(lexer), parser));
                BOOST_TEST(bp::parse("false" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
            {
                bool b = true;
                auto get_bool = [](auto & ctx) { return _globals(ctx); };
                auto parser = bp::with_globals(true_false(get_bool), b);
                BOOST_TEST(bp::parse("true" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("false" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
        }
        {
            constexpr auto lexer = bp::lexer<char, int> | true_false |
                                   identifier |
                                   bp::token_chars<'=', ';', '#', '$', '%'>;
            {
                constexpr auto parser = identifier;
                BOOST_TEST(bp::parse("func" | bp::to_tokens(lexer), parser));
                BOOST_TEST(bp::parse("foo" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
            {
                constexpr auto parser = identifier("func");
                BOOST_TEST(bp::parse("func" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("foo" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
            {
                constexpr auto parser = identifier("f\xC3\xBCnc");
                constexpr auto parser_u8 = identifier(u8"fünc");
                constexpr auto parser_u16 = identifier(u"fünc");
                constexpr auto parser_u32 = identifier(U"fünc");

                constexpr auto lexer_u8 =
                    bp::lexer<char8_t, int> | true_false | identifier |
                    bp::token_chars<'=', ';', '#', '$', '%'>;
                constexpr auto lexer_u16 =
                    bp::lexer<char16_t, int> | true_false | identifier |
                    bp::token_chars<'=', ';', '#', '$', '%'>;
                constexpr auto lexer_u32 =
                    bp::lexer<char32_t, int> | true_false | identifier |
                    bp::token_chars<'=', ';', '#', '$', '%'>;

                // There appears to be a bug in CTRE, related to the use of
                // char8_t.  It produces bad tokens, including appearances of
                // the replacement character.  The exact same input here
                // results in good tokens for all cases except char8_t input.
#if 0
                std::cout << "char tokens:\n";
                for (auto tok : "fünc" | bp::to_tokens(lexer)) {
                    std::cout << "tok=" << tok << "\n";
                }
                std::cout << "\n\n";

                std::cout << "char tokens:\n";
                for (auto tok : "f\xC3\xBCnc" | bp::to_tokens(lexer)) {
                    std::cout << "tok=" << tok << "\n";
                }
                std::cout << "\n\n";

                // BAD!
                std::cout << "char8_t tokens:\n";
                for (auto tok : u8"fünc" | bp::to_tokens(lexer_u8)) {
                    std::cout << "tok=" << tok << "\n";
                }
                std::cout << "\n\n";

                std::cout << "char16_t tokens:\n";
                for (auto tok : u"fünc" | bp::to_tokens(lexer_u16)) {
                    std::cout << "tok=" << tok << "\n";
                }
                std::cout << "\n\n";

                std::cout << "char32_t tokens:\n";
                for (auto tok : U"fünc" | bp::to_tokens(lexer_u32)) {
                    std::cout << "tok=" << tok << "\n";
                }
                std::cout << "\n\n";
#endif

                // Range to match is sequence of char; no transcoding will be
                // done.
                BOOST_TEST(
                    bp::parse("f\xC3\xBCnc" | bp::to_tokens(lexer), parser));
#if 0
                BOOST_TEST(
                    bp::parse(u8"fünc" | bp::to_tokens(lexer_u8), parser));
#endif
                BOOST_TEST(
                    bp::parse(u"fünc" | bp::to_tokens(lexer_u16), parser));
                BOOST_TEST(
                    bp::parse(U"fünc" | bp::to_tokens(lexer_u32), parser));

                BOOST_TEST( // char input; no transcoding on this one.
                    bp::parse("f\xC3\xBCnc" | bp::to_tokens(lexer), parser_u8));
#if 0
                BOOST_TEST(
                    bp::parse(u8"fünc" | bp::to_tokens(lexer_u8), parser_u8));
#endif
                BOOST_TEST(
                    bp::parse(u"fünc" | bp::to_tokens(lexer_u16), parser_u8));
                BOOST_TEST(
                    bp::parse(U"fünc" | bp::to_tokens(lexer_u32), parser_u8));

                BOOST_TEST( // char input; no transcoding on this one.
                    !bp::parse(
                        "f\xC3\xBCnc" | bp::to_tokens(lexer), parser_u16));
#if 0
                BOOST_TEST(
                    bp::parse(u8"fünc" | bp::to_tokens(lexer_u8), parser_u16));
#endif
                BOOST_TEST(
                    bp::parse(u"fünc" | bp::to_tokens(lexer_u16), parser_u16));
                BOOST_TEST(
                    bp::parse(U"fünc" | bp::to_tokens(lexer_u32), parser_u16));

                BOOST_TEST( // char input; no transcoding on this one.
                    !bp::parse(
                        "f\xC3\xBCnc" | bp::to_tokens(lexer), parser_u32));
#if 0
                BOOST_TEST(
                    bp::parse(u8"fünc" | bp::to_tokens(lexer_u8), parser_u32));
#endif
                BOOST_TEST(
                    bp::parse(u"fünc" | bp::to_tokens(lexer_u16), parser_u32));
                BOOST_TEST(
                    bp::parse(U"fünc" | bp::to_tokens(lexer_u32), parser_u32));

                BOOST_TEST(!bp::parse("func" | bp::to_tokens(lexer), parser));
                BOOST_TEST(
                    !bp::parse(u8"func" | bp::to_tokens(lexer_u8), parser));
                BOOST_TEST(
                    !bp::parse(u"func" | bp::to_tokens(lexer_u16), parser));
                BOOST_TEST(
                    !bp::parse(U"func" | bp::to_tokens(lexer_u32), parser));

                BOOST_TEST(
                    !bp::parse("func" | bp::to_tokens(lexer), parser_u8));
                BOOST_TEST(
                    !bp::parse(u8"func" | bp::to_tokens(lexer_u8), parser_u8));
                BOOST_TEST(
                    !bp::parse(u"func" | bp::to_tokens(lexer_u16), parser_u8));
                BOOST_TEST(
                    !bp::parse(U"func" | bp::to_tokens(lexer_u32), parser_u8));

                BOOST_TEST(
                    !bp::parse("func" | bp::to_tokens(lexer), parser_u16));
                BOOST_TEST(
                    !bp::parse(u8"func" | bp::to_tokens(lexer_u8), parser_u16));
                BOOST_TEST(
                    !bp::parse(u"func" | bp::to_tokens(lexer_u16), parser_u16));
                BOOST_TEST(
                    !bp::parse(U"func" | bp::to_tokens(lexer_u32), parser_u16));

                BOOST_TEST(
                    !bp::parse("func" | bp::to_tokens(lexer), parser_u32));
                BOOST_TEST(
                    !bp::parse(u8"func" | bp::to_tokens(lexer_u8), parser_u32));
                BOOST_TEST(
                    !bp::parse(u"func" | bp::to_tokens(lexer_u16), parser_u32));
                BOOST_TEST(
                    !bp::parse(U"func" | bp::to_tokens(lexer_u32), parser_u32));

                BOOST_TEST(!bp::parse("foo" | bp::to_tokens(lexer), parser));
                BOOST_TEST(!bp::parse("$" | bp::to_tokens(lexer), parser));
            }
        }
    }

    // basic
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        constexpr auto parser_1 = bp::char_ >> bp::char_;
        constexpr auto parser_2 = bp::char_ >> bp::char_ >> bp::char_;
        constexpr auto parser_3 = bp::char_ | bp::char_;
        constexpr auto parser_4 =
            bp::char_('#') | bp::char_('$') | bp::char_('%');
        constexpr auto parser_5 = bp::char_('#') | bp::char_('$') | bp::eps;

        {
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, bp::char_));
            BOOST_TEST(!bp::parse(r, bp::char_('$')));
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            char c = '\0';
            BOOST_TEST(bp::parse(r, bp::char_, c));
            BOOST_TEST(c == '#');
            BOOST_TEST(!bp::parse(r, bp::char_('$')));
        }
        {
            auto r = "$" | bp::to_tokens(lexer);
            char c = '\0';
            BOOST_TEST(bp::parse(r, bp::char_("#$"), c));
            BOOST_TEST(c == '$');
            BOOST_TEST(!bp::parse(r, bp::char_("%d")));
        }
        {
            auto r = "$" | bp::to_tokens(lexer);
            char c = '\0';
            std::string const pattern_1 = "#$";
            std::string const pattern_2 = "%d";
            BOOST_TEST(bp::parse(r, bp::char_(pattern_1), c));
            BOOST_TEST(c == '$');
            BOOST_TEST(!bp::parse(r, bp::char_(pattern_2)));
        }
        {
            auto r = "$" | bp::to_tokens(lexer);
            char c = '\0';
            BOOST_TEST(bp::parse(r, bp::char_('#', '$'), c));
            BOOST_TEST(c == '$');
            BOOST_TEST(!bp::parse(r, bp::char_('%', '%')));
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, bp::blank));
            BOOST_TEST(!bp::parse(r, bp::lower));
        }
        {
            auto r = "#$" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, bp::char_));
            {
                char const c_str[] = "#$";
                auto r = bp::null_term(c_str) | bp::to_tokens(lexer);
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), bp::char_));
            }
            BOOST_TEST(bp::parse(r, parser_1));
            BOOST_TEST(!bp::parse(r, parser_2));
        }
        {
            auto r = "#$" | bp::to_tokens(lexer);
            bp::tuple<char, char> result;
            BOOST_TEST(bp::parse(r, parser_1, result));
            using namespace bp::literals;
            BOOST_TEST(bp::get(result, 0_c) == '$');
            BOOST_TEST(bp::get(result, 1_c) == '\0');
        }
        {
            auto r = "#$%" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser_1));
            {
                char const c_str[] = "#$";
                auto r = bp::null_term(c_str) | bp::to_tokens(lexer);
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser_1));
            }
            BOOST_TEST(bp::parse(r, parser_2));
        }
        {
            auto r = "#$%" | bp::to_tokens(lexer);
            bp::tuple<char, char, char> result;
            BOOST_TEST(bp::parse(r, parser_2, result));
            using namespace bp::literals;
            BOOST_TEST(bp::get(result, 0_c) == '%');
            BOOST_TEST(bp::get(result, 1_c) == '\0');
            BOOST_TEST(bp::get(result, 2_c) == '\0');
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser_3));
            BOOST_TEST(bp::parse(r, parser_4));
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            char c = '\0';
            BOOST_TEST(bp::parse(r, parser_3, c));
            BOOST_TEST(c == '#');
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            char c = '\0';
            BOOST_TEST(bp::parse(r, parser_4, c));
            BOOST_TEST(c == '#');
        }
        {
            auto r = ";" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser_3));
            BOOST_TEST(!bp::parse(r, parser_4));
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser_5));
        }
        {
            auto r = "z" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser_5));
            {
                char const c_str[] = "z";
                auto r = bp::null_term(c_str) | bp::to_tokens(lexer);
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser_5));
            }
        }
        {
            auto r = "#" | bp::to_tokens(lexer);
            std::optional<char> c;
            BOOST_TEST(bp::parse(r, parser_5, c));
            BOOST_TEST(c == '#');
        }
        {
            auto r = "z" | bp::to_tokens(lexer);
            std::optional<char> c;
            BOOST_TEST(!bp::parse(r, parser_5, c));
        }
        {
            std::optional<char> c;
            char const c_str[] = "z";
            auto r = bp::null_term(c_str) | bp::to_tokens(lexer);
            auto first = r.begin();
            BOOST_TEST(bp::prefix_parse(first, r.end(), parser_5, c));
            BOOST_TEST(c == std::nullopt);
        }
    }

    // star
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = *bp::char_;
            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>());
            }
            {
                auto r = "#" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'#'}));
            }
            {
                auto r = "$#" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$', '#'}));
            }
        }

        {
            constexpr auto parser = *bp::char_('$');
            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>());
            }
            {
                auto r = "$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$'}));
            }
            {
                auto r = "$$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$', '$'}));
            }
        }
    }

    // plus
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = +bp::char_;

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }
            {
                auto r = "#" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'#'}));
            }
            {
                auto r = "$#" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$', '#'}));
            }
        }

        {
            constexpr auto parser = +bp::char_('$');

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }
            {
                auto r = "$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$'}));
            }
            {
                auto r = "$$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$', '$'}));
            }
        }
    }

    // star_and_plus_collapsing
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = +(+bp::char_('$'));

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }
            {
                auto r = "$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$'}));
            }
            {
                auto r = "$$" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({'$', '$'}));
            }
        }

        {
            constexpr auto parser = **bp::char_(';');

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>());
            }
            {
                auto r = ";" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';'}));
            }
            {
                auto r = ";;" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';', ';'}));
            }
        }

        {
            constexpr auto parser = +*bp::char_(';');

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>());
            }
            {
                auto r = ";" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';'}));
            }
            {
                auto r = ";;" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';', ';'}));
            }
        }

        {
            constexpr auto parser = *+bp::char_(';');

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>());
            }
            {
                auto r = ";" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';'}));
            }
            {
                auto r = ";;" | bp::to_tokens(lexer);
                std::vector<char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<char>({';', ';'}));
            }
        }
    }

    // action_
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            {
                auto r = "" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = *bp::char_('$')[action];
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(ss.str() == "");
            }
            {
                auto r = "$" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = *bp::char_('$')[action];
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(ss.str() == "$");
            }
            {
                auto r = "$$" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = *bp::char_('$')[action];
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(ss.str() == "$$$$");
            }
        }

        {
            {
                auto r = "" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = +bp::char_('$')[action];
                BOOST_TEST(!bp::parse(r, parser));
                BOOST_TEST(ss.str() == "");
            }
            {
                auto r = "$" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = +bp::char_('$')[action];
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(ss.str() == "$");
            }
            {
                auto r = "$$" | bp::to_tokens(lexer);
                std::stringstream ss;
                auto action = [&ss](auto & ctx) { ss << _attr(ctx); };
                auto parser = +bp::char_('$')[action];
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(bp::parse(r, parser));
                BOOST_TEST(ss.str() == "$$$$");
            }
        }
    }

    // transform
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        int calls = 0;
        auto str_sum = [&](std::string_view s) {
            ++calls;
            int retval = 0;
            for (auto ch : s) {
                retval += ch - 'a';
            }
            return retval;
        };

        {
            constexpr auto parser = identifier;
            auto r = "abcdef" | bp::to_tokens(lexer);
            {
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == "abcdef");
            }
            {
                calls = 0;
                auto result = bp::parse(r, bp::transform(str_sum)[parser]);
                BOOST_TEST(result);
                BOOST_TEST(*result == 15);
                BOOST_TEST(calls == 1);
            }
            {
                calls = 0;
                auto result = bp::parse(r, bp::transform(str_sum)[parser]);
                BOOST_TEST(result);
                BOOST_TEST(*result == 15);
                BOOST_TEST(calls == 1);
            }
            {
                calls = 0;
                auto result = bp::parse(r, bp::transform(str_sum)[parser]);
                BOOST_TEST(result);
                BOOST_TEST(*result == 15);
                BOOST_TEST(calls == 1);
            }
        }
        {
            constexpr auto parser = identifier;
            auto r = "abcdef" | bp::to_tokens(lexer);
            {
                calls = 0;
                auto result =
                    bp::parse(r, bp::omit[bp::transform(str_sum)[parser]]);
                BOOST_TEST(result);
                BOOST_TEST(calls == 0);
            }
            {
                calls = 0;
                auto result =
                    bp::parse(r, bp::omit[bp::transform(str_sum)[parser]]);
                BOOST_TEST(result);
                BOOST_TEST(calls == 0);
            }
            {
                calls = 0;
                auto result =
                    bp::parse(r, bp::omit[bp::transform(str_sum)[parser]]);
                BOOST_TEST(result);
                BOOST_TEST(calls == 0);
            }
        }
    }

    // omit
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = bp::omit[*+bp::char_(';')];

            {
                auto r = "" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = ";" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = ";;" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = "" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = ";" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = ";;" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
        }

        {
            constexpr auto parser = bp::omit[*bp::string("ab")];

            {
                auto r = "" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = "a" | bp::to_tokens(lexer);
                BOOST_TEST(!bp::parse(r, parser));
            }
            {
                char const c_str[] = "a";
                auto r = bp::null_term(c_str) | bp::to_tokens(lexer);
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser));
            }
            {
                auto r = "ab" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
            {
                auto r = "ab ab" | bp::to_tokens(lexer);
                BOOST_TEST(bp::parse(r, parser));
            }
        }
    }

    // repeat
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = bp::repeat(2, 3)[bp::string("zs")];

            {
                auto r = "" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                auto r = "z" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                auto r = "zs" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                auto r = "zs zs" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"zs", "zs"}));

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(
                        *chars == std::vector<std::string>({"zs", "zs"}));
                }
            }
        }
        {
            constexpr auto parser = *bp::char_ >> bp::eps >> *bp::string("str");
            auto result = bp::parse("#$%str" | bp::to_tokens(lexer), parser);
            BOOST_TEST(result);
            BOOST_TEST(*result == std::vector<std::string>({"#$%", "str"}));
        }
        {
            constexpr auto parser =
                *(bp::char_ - "str") >> bp::eps >> *bp::string("str");
            auto result =
                bp::parse("#$% str str" | bp::to_tokens(lexer), parser);
            BOOST_TEST(result);
            BOOST_TEST(
                *result == std::vector<std::string>({"#$%", "str", "str"}));
        }
    }

    // raw
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<'=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = bp::raw[*bp::string("zs")];
            auto const dummy_r = "" | bp::to_tokens(lexer);
            using range_t =
                BOOST_PARSER_DETAIL_TEXT_SUBRANGE<decltype(dummy_r.begin())>;

            {
                char const c_str[] = "";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result.begin() == r.begin());
                BOOST_TEST(result.end() == r.begin());
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(!bp::parse(r, parser, result));
                BOOST_TEST(result.begin() == result.end());
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
                BOOST_TEST(result.begin() == r.begin());
                BOOST_TEST(result.end() == r.begin());
            }
            {
                char const c_str[] = "zs";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result.begin() == r.begin());
                BOOST_TEST(result.end() == r.end());
            }
            {
                char const c_str[] = "zs zs";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result.begin() == r.begin());
                BOOST_TEST(result.end() == r.end());
            }
            {
                char const c_str[] = "";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(result->begin() == r.begin());
                BOOST_TEST(result->end() == r.begin());
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(!result);
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                auto first = r.begin();
                std::optional<range_t> result =
                    bp::prefix_parse(first, r.end(), parser);
                BOOST_TEST(result);
                BOOST_TEST(result->begin() == r.begin());
                BOOST_TEST(result->end() == r.begin());
            }
            {
                char const c_str[] = "zs";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(result->begin() == r.begin());
                BOOST_TEST(result->end() == r.end());
            }
            {
                char const c_str[] = "zs zs";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(result->begin() == r.begin());
                BOOST_TEST(result->end() == r.end());
            }
        }
    }

    // string_view
    {
        {
            constexpr auto lexer = bp::lexer<char, int> | true_false |
                                   identifier |
                                   bp::token_chars<'=', ';', '#', '$', '%'>;

            constexpr auto parser = bp::string_view[*bp::string("zs")];
            using range_t = std::string_view;

            {
                char const c_str[] = "";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == "");
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(!bp::parse(r, parser, result));
                BOOST_TEST(result == "");
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
                BOOST_TEST(result == "");
            }
            {
                char const c_str[] = "zs";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == "zs");
            }
            {
                char const c_str[] = "zs zs";
                auto r = c_str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == "zs zs");
            }
            {
                char const c_str[] = "";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == "");
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(!result);
            }
            {
                char const c_str[] = "z";
                auto r = c_str | bp::to_tokens(lexer);
                auto first = r.begin();
                std::optional<range_t> result =
                    bp::prefix_parse(first, r.end(), parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == "");
            }
            {
                char const c_str[] = "zs";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == "zs");
            }
            {
                char const c_str[] = "zs zs";
                auto r = c_str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == "zs zs");
            }
        }
        {
            constexpr auto lexer = bp::lexer<char32_t, int> | true_false |
                                   identifier |
                                   bp::token_chars<'=', ';', '#', '$', '%'>;

            constexpr auto parser = bp::string_view[*bp::string("zs")];
            using range_t = std::u32string_view;

            {
                std::u32string const str = U"";
                auto r = str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == U"");
            }
            {
                std::u32string const str = U"z";
                auto r = str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(!bp::parse(r, parser, result));
                BOOST_TEST(result == U"");
            }
            {
                std::u32string const str = U"z";
                auto r = str | bp::to_tokens(lexer);
                range_t result;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
                BOOST_TEST(result == U"");
            }
            {
                std::u32string const str = U"zs";
                auto r = str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == U"zs");
            }
            {
                std::u32string const str = U"zs zs";
                auto r = str | bp::to_tokens(lexer);
                range_t result;
                BOOST_TEST(bp::parse(r, parser, result));
                BOOST_TEST(result == U"zs zs");
            }
            {
                std::u32string const str = U"";
                auto r = str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == U"");
            }
            {
                std::u32string const str = U"z";
                auto r = str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(!result);
            }
            {
                std::u32string const str = U"z";
                auto r = str | bp::to_tokens(lexer);
                auto first = r.begin();
                std::optional<range_t> result =
                    bp::prefix_parse(first, r.end(), parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == U"");
            }
            {
                std::u32string const str = U"zs";
                auto r = str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == U"zs");
            }
            {
                std::u32string const str = U"zs zs";
                auto r = str | bp::to_tokens(lexer);
                std::optional<range_t> result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(*result == U"zs zs");
            }
        }
    }

    // delimited
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<',', '=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = bp::string("yay") % ',';

            {
                std::string const str = "";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                std::string const str = "z";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                std::string const str = ",";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                std::string const str = ",yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
            }
            {
                std::string const str = "yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"yay"}));

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
                }
            }
            {
                std::string const str = "yay yay";
                auto r = str | bp::to_tokens(lexer);
                {
                    std::vector<std::string> chars;
                    BOOST_TEST(!bp::parse(r, parser, chars));
                    BOOST_TEST(chars == std::vector<std::string>{});
                }
                {
                    std::vector<std::string> chars;
                    auto first = str.c_str();
                    BOOST_TEST(bp::prefix_parse(
                        first, bp::detail::text::null_sentinel, parser, chars));
                    BOOST_TEST(chars == std::vector<std::string>({"yay"}));
                }

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
                {
                    auto first = str.c_str();
                    std::optional<std::vector<std::string>> const chars =
                        bp::prefix_parse(
                            first, bp::detail::text::null_sentinel, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
                }
            }
            {
                std::string const str = "yay,";
                auto r = str | bp::to_tokens(lexer);
                {
                    std::vector<std::string> chars;
                    BOOST_TEST(!bp::parse(r, parser, chars));
                }
                {
                    std::vector<std::string> chars;
                    auto first = str.c_str();
                    BOOST_TEST(bp::prefix_parse(
                        first, bp::detail::text::null_sentinel, parser, chars));
                    BOOST_TEST(chars == std::vector<std::string>({"yay"}));
                }

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
                {
                    auto first = str.c_str();
                    std::optional<std::vector<std::string>> const chars =
                        bp::prefix_parse(
                            first, bp::detail::text::null_sentinel, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
                }
            }
            {
                std::string const str = "yay,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));

                {
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(
                        *chars ==
                        std::vector<std::string>({"yay", "yay", "yay"}));
                }
            }
        }

        {
            constexpr auto parser = bp::string("yay") % ',';
            {
                std::string const str = "";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }

            {
                std::string const str = "";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = "z";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = "z";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = ",";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = ",";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = " ,yay";
                auto r = " ,yay" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = " ,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = ", yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = " ,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = ",yay ";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = ",yay ";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }

            {
                std::string const str = " , yay ";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>{});
            }
            {
                std::string const str = " , yay ";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = "yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
            }
            {
                std::string const str = "yay yay";
                auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }
            {
                std::string const str = "yay yay";
                auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = "yay yay";
                auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                auto first = r.begin();
                std::optional<std::vector<std::string>> const chars =
                    bp::prefix_parse(first, r.end(), parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay,";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }
            {
                std::string const str = "yay,";
                auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                auto first = r.begin();
                BOOST_TEST(bp::prefix_parse(first, r.end(), parser, chars));
                BOOST_TEST(chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay,";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(!chars);
            }
            {
                std::string const str = "yay,";
                auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                auto first = r.begin();
                std::optional<std::vector<std::string>> const chars =
                    bp::prefix_parse(first, r.end(), parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
            }
            {
                std::string const str = "yay,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = " yay,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = " yay,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay ,yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = "yay ,yay,yay";
                auto r = "yay ,yay,yay" | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay, yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay, yay,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay,yay ,yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = "yay,yay ,yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay,yay, yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = "yay,yay, yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay,yay,yay ";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = "yay,yay,yay ";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = " yay , yay , yay ";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = " yay , yay , yay ";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
            {
                std::string const str = "yay, yay, yay";
                auto r = str | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }

            {
                std::string const str = "yay, yay, yay";
                auto r = str | bp::to_tokens(lexer);
                std::optional<std::vector<std::string>> const chars =
                    bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(
                    *chars == std::vector<std::string>({"yay", "yay", "yay"}));
            }
        }

        {
            constexpr auto yay = bp::string("yay") % ',';
            constexpr auto aww = bp::string("aww") % ',';
            constexpr auto parser = yay >> ',' >> aww;

            {
                auto r = "yay, yay, yay, aww, aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(
                    *result ==
                    (bp::tuple<
                        std::vector<std::string>,
                        std::vector<std::string>>(
                        std::vector<std::string>({"yay", "yay", "yay"}),
                        std::vector<std::string>({"aww", "aww"}))));
            }
            {
                auto r = "yay, yay, yay , aww, aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                BOOST_TEST(
                    *result ==
                    (bp::tuple<
                        std::vector<std::string>,
                        std::vector<std::string>>(
                        std::vector<std::string>({"yay", "yay", "yay"}),
                        std::vector<std::string>({"aww", "aww"}))));
            }
        }

        {
            constexpr auto yay = bp::string("yay") % ',';
            constexpr auto aww = bp::string("aww") % ',';
            constexpr auto parser = bp::raw[yay] >> ',' >> bp::raw[aww];

            using namespace bp::literals;

            {
                auto r = "yay, yay, yay, aww, aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                auto subrange_0 = bp::get(*result, 0_c);
                auto subrange_1 = bp::get(*result, 1_c);
                BOOST_TEST(subrange_0.begin() == r.begin());
                BOOST_TEST(subrange_0.end() == std::next(r.begin(), 5));
                BOOST_TEST(subrange_1.begin() == std::next(r.begin(), 6));
                BOOST_TEST(subrange_1.end() == std::next(r.begin(), 9));
            }
            {
                auto r = "yay, yay, yay , aww, aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                auto subrange_0 = bp::get(*result, 0_c);
                auto subrange_1 = bp::get(*result, 1_c);
                BOOST_TEST(subrange_0.begin() == r.begin());
                BOOST_TEST(subrange_0.end() == std::next(r.begin(), 5));
                BOOST_TEST(subrange_1.begin() == std::next(r.begin(), 6));
                BOOST_TEST(subrange_1.end() == std::next(r.begin(), 9));
            }
        }

        {
            constexpr auto yay = *bp::string("yay");
            constexpr auto aww = *bp::string("aww");
            constexpr auto parser = bp::raw[yay] >> ',' >> bp::raw[aww];

            {
                auto r = "yay yay yay, aww aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                auto subrange_0 = bp::get(*result, bp::llong<0>{});
                auto subrange_1 = bp::get(*result, bp::llong<1>{});
                BOOST_TEST(subrange_0.begin() == r.begin());
                BOOST_TEST(subrange_0.end() == std::next(r.begin(), 3));
                BOOST_TEST(subrange_1.begin() == std::next(r.begin(), 4));
                BOOST_TEST(subrange_1.end() == std::next(r.begin(), 6));
            }
            {
                auto r = "yay yay yay , aww aww" | bp::to_tokens(lexer);
                auto result = bp::parse(r, parser);
                BOOST_TEST(result);
                auto subrange_0 = bp::get(*result, bp::llong<0>{});
                auto subrange_1 = bp::get(*result, bp::llong<1>{});
                BOOST_TEST(subrange_0.begin() == r.begin());
                BOOST_TEST(subrange_0.end() == std::next(r.begin(), 3));
                BOOST_TEST(subrange_1.begin() == std::next(r.begin(), 4));
                BOOST_TEST(subrange_1.end() == std::next(r.begin(), 6));
            }
        }
    }

    // lexeme
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<',', '=', ';', '#', '$', '%'>;

        {
            constexpr auto parser = bp::lexeme[bp::string("yay") % ','];

            {
                {
                    auto r = "yay, yay, yay" | bp::to_tokens(lexer);
                    std::vector<std::string> chars;
                    BOOST_TEST(!bp::parse(r, parser, chars));
                }
                {
                    std::string const str = "yay, yay, yay";
                    auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                    std::vector<std::string> chars;
                    auto first = r.begin();
                    BOOST_TEST(bp::prefix_parse(first, r.end(), parser, chars));
                    BOOST_TEST(chars == std::vector<std::string>({"yay"}));
                }

                {
                    auto r = "yay, yay, yay" | bp::to_tokens(lexer);
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(!chars);
                }
                {
                    std::string const str = "yay, yay, yay";
                    auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
                    auto first = r.begin();
                    std::optional<std::vector<std::string>> const chars =
                        bp::prefix_parse(first, r.end(), parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(*chars == std::vector<std::string>({"yay"}));
                }
            }
        }

        {
            constexpr auto parser =
                bp::lexeme[bp::skip[bp::string("yay") % ',']];

            {
                auto r = "yay, yay, yay" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));

                {
                    auto r = "yay, yay, yay" | bp::to_tokens(lexer);
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(
                        *chars ==
                        std::vector<std::string>({"yay", "yay", "yay"}));
                }
            }
            {
                auto r = " yay, yay, yay" | bp::to_tokens(lexer);
                std::vector<std::string> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(
                    chars == std::vector<std::string>({"yay", "yay", "yay"}));

                {
                    auto r = " yay, yay, yay" | bp::to_tokens(lexer);
                    std::optional<std::vector<std::string>> const chars =
                        bp::parse(r, parser);
                    BOOST_TEST(chars);
                    BOOST_TEST(
                        *chars ==
                        std::vector<std::string>({"yay", "yay", "yay"}));
                }
            }
        }
    }

    // combined_seq_and_or
    {
        constexpr auto lexer = bp::lexer<char, int> | true_false | identifier |
                               bp::token_chars<',', '=', ';', '#', '$', '%'>;

        {
            constexpr auto parser =
                bp::char_('#') >> bp::char_('$') >> bp::char_('%') |
                bp::char_(',') >> bp::char_('=') >> bp::char_(';');
            using tup = bp::tuple<char, char, char>;

            {
                auto r = "#$%" | bp::to_tokens(lexer);
                bp::tuple<char, char, char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == tup('%', '\0', '\0'));
            }

            {
                auto r = "#$%" | bp::to_tokens(lexer);
                std::optional<std::string> const chars = bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == "#$%");
            }

            {
                auto r = ",=;" | bp::to_tokens(lexer);
                tup chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == tup(';', '\0', '\0'));
            }
        }

        {
            constexpr auto parser =
                bp::char_('#') >> bp::string("q") >> bp::char_('%') |
                bp::char_(',') >> bp::string("r") >> bp::char_(';');
            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == "#q%");
            }

            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::optional<std::string> const chars = bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == "#q%");
            }

            {
                auto r = ",r;" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == ",r;");
            }
        }

        {
            constexpr auto parser =
                bp::char_('#') >> bp::char_('$') >> bp::char_('%') |
                bp::char_(',') >> bp::char_('=') >> bp::char_(';');
            using tup = bp::tuple<char, char, char>;

            {
                auto r = "#$%" | bp::to_tokens(lexer);
                bp::tuple<std::any, std::any, std::any> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
            }

            {
                auto r = ",=;" | bp::to_tokens(lexer);
                bp::tuple<char, char, char> chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == tup(';', '\0', '\0'));
            }
        }

        {
            constexpr auto parser = !bp::char_('#');
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser));
        }

        {
            constexpr auto parser = &bp::char_('#');
            auto r = "#" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser));
        }
        {
            constexpr auto parser = &bp::char_('#');
            std::string const str = "#";
            auto r = bp::null_term(str.c_str()) | bp::to_tokens(lexer);
            auto first = r.begin();
            BOOST_TEST(bp::prefix_parse(first, r.end(), parser));
        }

        {
#if defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Woverloaded-shift-op-parentheses"
#endif
            constexpr auto parser =
                (bp::char_('#') >> bp::string("q") > bp::char_('%')) |
                (bp::char_(',') >> bp::string("r") >> bp::char_(';'));
#if defined(__clang__)
#pragma GCC diagnostic pop
#endif
            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == "#q%");
            }

            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::optional<std::string> const chars = bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == "#q%");
            }

            {
                auto r = ",r;" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == ",r;");
            }

            {
                auto const r = "#q;" | bp::to_tokens(lexer);
                std::string chars;
                bp::rethrow_error_handler eh;
                BOOST_TEST_THROWS(
                    bp::parse(r, bp::with_error_handler(parser, eh), chars),
                    bp::parse_error<decltype(r.begin())>);
            }

            {
                auto r = "#q;" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(!bp::parse(r, parser, chars));
            }

            {
                auto r = "#q;" | bp::to_tokens(lexer);
                std::string chars;
                bp::stream_error_handler eh("simple_parser.cpp");
                BOOST_TEST(
                    !bp::parse(r, bp::with_error_handler(parser, eh), chars));
            }

            {
                auto r = "#q" | bp::to_tokens(lexer);
                std::string chars;
                bp::stream_error_handler eh("simple_parser.cpp");
                BOOST_TEST(
                    !bp::parse(r, bp::with_error_handler(parser, eh), chars));
            }
        }

        {
#if defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Woverloaded-shift-op-parentheses"
#endif
            constexpr auto parser =
                (bp::char_('#') >> bp::string("q") > bp::char_('%')) |
                (bp::char_(',') >> bp::string("r") >> bp::char_(';'));
#if defined(__clang__)
#pragma GCC diagnostic pop
#endif
            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == "#q%");
            }

            {
                auto r = "#q%" | bp::to_tokens(lexer);
                std::optional<std::string> const chars = bp::parse(r, parser);
                BOOST_TEST(chars);
                BOOST_TEST(*chars == "#q%");
            }

            {
                auto r = ",r;" | bp::to_tokens(lexer);
                std::string chars;
                BOOST_TEST(bp::parse(r, parser, chars));
                BOOST_TEST(chars == ",r;");
            }
        }
    }

    // eol_
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<
                                                          'y',
                                                          '\x0009',
                                                          '\x000a',
                                                          '\x000b',
                                                          '\x000c',
                                                          '\x000d'>;

        constexpr auto parser = bp::eol;

        {
            auto r = "y" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser));
        }
        {
            auto r = "\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000d\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000b" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000c" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000d" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
    }

    // ws_
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<
                                                          'y',
                                                          '\x0009',
                                                          '\x000a',
                                                          '\x000b',
                                                          '\x000c',
                                                          '\x000d'>;

        constexpr auto parser = bp::ws;

        {
            auto r = "y" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser));
        }
        {
            auto r = "\x0009" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000d\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000b" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000c" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
        {
            auto r = "\x000d" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser));
        }
    }

    // blank_
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<
                                                          'y',
                                                          '\x0009',
                                                          '\x000a',
                                                          '\x000b',
                                                          '\x000c',
                                                          '\x000d'>;

        constexpr auto parser = bp::blank;
        constexpr auto alt_parser = bp::ws - bp::eol;

        {
            auto r = "y" | bp::to_tokens(lexer);
            BOOST_TEST(!bp::parse(r, parser));
        }
        {
            auto r = "\x0009" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
        {
            auto r = "\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
        {
            auto r = "\x000d\x000a" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
        {
            auto r = "\x000b" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
        {
            auto r = "\x000c" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
        {
            auto r = "\x000d" | bp::to_tokens(lexer);
            BOOST_TEST(bp::parse(r, parser) == bp::parse(r, alt_parser));
        }
    }

    // digit_
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'0', '1', 'a'>;

        constexpr auto parser = +bp::digit;

        auto r = "0 1" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'0', '1'}));
    }
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'0', '1', 'a'>;

        constexpr auto parser = +bp::digit;

        auto r = "0 a 1" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'0'}));
    }

    // hex_digit_
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'0', 'a', 'z'>;

        constexpr auto parser = +bp::hex_digit;

        auto r = "0 a" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'0', 'a'}));
    }
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'0', 'a', 'z'>;

        constexpr auto parser = +bp::hex_digit;

        auto r = "0 z a" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'0'}));
    }

    // control_
    {
        constexpr auto lexer =
            bp::lexer<char, int> |
            bp::token_chars<'\x0001', '\x001f', '\x007f', 'a'>;

        constexpr auto parser = +bp::control;

        auto r = "\x0001 \x001f \x007f" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({1, 0x001f, 0x007f}));
    }
    {
        constexpr auto lexer =
            bp::lexer<char, int> |
            bp::token_chars<'\x0001', '\x001f', '\x007f', 'a'>;

        constexpr auto parser = +bp::control;

        auto r = "\x0001 a \x001f \x007f" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({1}));
    }

    // punct_
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'\x0021', 'a'>;

        auto parser = +bp::punct;

        auto r = "\x0021 \x0021" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({0x21, 0x21}));
    }
    {
        constexpr auto lexer =
            bp::lexer<char, int> | bp::token_chars<'\x0021', 'a'>;

        auto parser = +bp::punct;

        auto r = "\x0021 a \x0021" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({0x21}));
    }

    // lower_
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<'A', 'a'>;

        auto parser = +bp::lower;

        auto r = "aa" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'a', 'a'}));
    }
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<'A', 'a'>;

        auto parser = +bp::lower;

        auto r = "aA" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'a'}));
    }

    // upper_
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<'A', 'a'>;

        auto parser = +bp::upper;

        auto r = "AA" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'A', 'A'}));
    }
    {
        constexpr auto lexer = bp::lexer<char, int> | bp::token_chars<'A', 'a'>;

        auto parser = +bp::upper;

        auto r = "Aa" | bp::to_tokens(lexer);
        std::vector<uint32_t> result;
        auto first = r.begin();
        BOOST_TEST(bp::prefix_parse(first, r.end(), parser, result));
        BOOST_TEST(result == std::vector<uint32_t>({'A'}));
    }

    return boost::report_errors();
}
