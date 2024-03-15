/**
 *   Copyright (C) 2024 T. Zachary Laine
 *
 *   Distributed under the Boost Software License, Version 1.0. (See
 *   accompanying file LICENSE_1_0.txt or copy at
 *   http://www.boost.org/LICENSE_1_0.txt)
 */

#include <boost/parser/search.hpp>

#include <gtest/gtest.h>


namespace bp = boost::parser;

#if BOOST_PARSER_USE_CONCEPTS
namespace deduction {
    std::string str;
    auto const parser = bp::char_;
    auto const skip = bp::ws;

    auto deduced_1 = bp::search_all_view(str, parser, skip, bp::trace::on);
    auto deduced_2 = bp::search_all_view(str, parser, skip);
    auto deduced_3 = bp::search_all_view(str, parser, bp::trace::on);
    auto deduced_4 = bp::search_all_view(str, parser);
}
#endif

TEST(search, search_range_skip)
{
    // array of char
    {
        char const str[] = "";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const str[] = "not here";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::end(str) - 1);
        EXPECT_EQ(result.end(), std::end(str) - 1);
    }
    {
        char const str[] = "aaXYZb";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZab";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "gbXYZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "XXYZZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }
    {
        char const * str = "XXYZZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }
#if BOOST_PARSER_USE_CONCEPTS
    {
        auto result = bp::search(std::string("XXYZZ"), bp::lit("XYZ"), bp::ws);
        static_assert(std::same_as<decltype(result), std::ranges::dangling>);
    }
#endif

    // array of char | as_utf32
    {
        char const str[] = "";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const str[] = "not here";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::end(str) - 1);
        EXPECT_EQ(result.end(), std::end(str) - 1);
    }
    {
        char const str[] = "aaXYZb";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZab";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "gbXYZ";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZ";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "XXYZZ";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str = "";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const * str = "not here";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_TRUE(result.begin() == bp::null_sentinel_t{});
        EXPECT_TRUE(result.end() == bp::null_sentinel_t{});
    }
    {
        char const * str = "aaXYZb";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const * str = "XYZab";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const * str = "gbXYZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const * str = "XYZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const * str = "XXYZZ";
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str_ = "";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str.begin());
        EXPECT_EQ(result.end(), str.end());
    }
    {
        char const * str_ = "not here";
        auto str = str_ | bp::as_utf16;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_TRUE(result.begin() == str.end());
        EXPECT_TRUE(result.end() == str.end());
    }
    {
        char const * str_ = "aaXYZb";
        auto str = str_ | bp::as_utf8;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 2));
        EXPECT_EQ(result.end(), std::next(str.begin(), 5));
    }
    {
        char const * str_ = "XYZab";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
    {
        char const * str_ = "gbXYZ";
        auto str = str_ | bp::as_utf16;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 2));
        EXPECT_EQ(result.end(), std::next(str.begin(), 5));
    }
    {
        char const * str_ = "XYZ";
        auto str = str_ | bp::as_utf8;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
    {
        char const * str_ = "XXYZZ";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 1));
        EXPECT_EQ(result.end(), std::next(str.begin(), 4));
    }
}

TEST(search, search_iters_skip)
{
    // array of char
    {
        char const str[] = "XYZab";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "gbXYZ";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZ";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }

    // array of char | as_utf32
    {
        char const str_[] = "";
        auto str = str_ | bp::as_utf32;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str_);
        EXPECT_EQ(result.end(), str_);
    }
    {
        char const str_[] = "XYZ";
        auto str = str_ | bp::as_utf32;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str_ + 0);
        EXPECT_EQ(result.end(), str_ + 3);
    }
    {
        char const str_[] = "XXYZZ";
        auto str = str_ | bp::as_utf32;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str_ + 1);
        EXPECT_EQ(result.end(), str_ + 4);
    }

    // pointer
    {
        char const * str = "";
        auto result =
            bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const * str = "not here";
        auto result =
            bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"), bp::ws);
        EXPECT_TRUE(result.begin() == bp::null_sentinel_t{});
        EXPECT_TRUE(result.end() == bp::null_sentinel_t{});
    }
    {
        char const * str = "XXYZZ";
        auto result =
            bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str_ = "XYZab";
        auto str = str_ | bp::as_utf32;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
    {
        char const * str_ = "gbXYZ";
        auto str = str_ | bp::as_utf16;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 2));
        EXPECT_EQ(result.end(), std::next(str.begin(), 5));
    }
    {
        char const * str_ = "XYZ";
        auto str = str_ | bp::as_utf8;
        auto result =
            bp::search(str.begin(), str.end(), bp::lit("XYZ"), bp::ws);
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
}

TEST(search, search_range_no_skip)
{
    // array of char
    {
        char const str[] = "XYZab";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "gbXYZ";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZ";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }

    // array of char | as_utf32
    {
        char const str[] = "";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const str[] = "XYZ";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "XXYZZ";
        auto result = bp::search(str | bp::as_utf32, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str = "";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const * str = "not here";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_TRUE(result.begin() == bp::null_sentinel_t{});
        EXPECT_TRUE(result.end() == bp::null_sentinel_t{});
    }
    {
        char const * str = "XXYZZ";
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str_ = "XYZab";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
    {
        char const * str_ = "gbXYZ";
        auto str = str_ | bp::as_utf16;
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 2));
        EXPECT_EQ(result.end(), std::next(str.begin(), 5));
    }
    {
        char const * str_ = "XYZ";
        auto str = str_ | bp::as_utf8;
        auto result = bp::search(str, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
}

TEST(search, search_iters_no_skip)
{
    // array of char
    {
        char const str[] = "XYZab";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }
    {
        char const str[] = "gbXYZ";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 2);
        EXPECT_EQ(result.end(), str + 5);
    }
    {
        char const str[] = "XYZ";
        auto result =
            bp::search(std::begin(str), std::end(str), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 0);
        EXPECT_EQ(result.end(), str + 3);
    }

    // array of char | as_utf32
    {
        char const str_[] = "";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str_);
        EXPECT_EQ(result.end(), str_);
    }
    {
        char const str_[] = "XYZ";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str_ + 0);
        EXPECT_EQ(result.end(), str_ + 3);
    }
    {
        char const str_[] = "XXYZZ";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str_ + 1);
        EXPECT_EQ(result.end(), str_ + 4);
    }

    // pointer
    {
        char const * str = "";
        auto result = bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str);
        EXPECT_EQ(result.end(), str);
    }
    {
        char const * str = "not here";
        auto result = bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"));
        EXPECT_TRUE(result.begin() == bp::null_sentinel_t{});
        EXPECT_TRUE(result.end() == bp::null_sentinel_t{});
    }
    {
        char const * str = "XXYZZ";
        auto result = bp::search(str, bp::null_sentinel_t{}, bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), str + 1);
        EXPECT_EQ(result.end(), str + 4);
    }

    // pointer
    {
        char const * str_ = "XYZab";
        auto str = str_ | bp::as_utf32;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
    {
        char const * str_ = "gbXYZ";
        auto str = str_ | bp::as_utf16;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 2));
        EXPECT_EQ(result.end(), std::next(str.begin(), 5));
    }
    {
        char const * str_ = "XYZ";
        auto str = str_ | bp::as_utf8;
        auto result = bp::search(str.begin(), str.end(), bp::lit("XYZ"));
        EXPECT_EQ(result.begin(), std::next(str.begin(), 0));
        EXPECT_EQ(result.end(), std::next(str.begin(), 3));
    }
}

TEST(search, search_all)
{
    {
        auto r = bp::search_all("", bp::lit("XYZ"), bp::ws);
        int count = 0;
        for (auto subrange : r) {
            (void)subrange;
            ++count;
        }
        EXPECT_EQ(count, 0);
    }
    {
        char const str[] = "aaXYZb";
        auto r = bp::search_all(str, bp::lit("XYZ"), bp::ws);
        int count = 0;
        int const offsets[] = {2, 5};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 1);
    }
    {
        char const str[] = "aaXYZbaabaXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"), bp::ws, bp::trace::off);
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str[] = "aaXYZbaabaXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"), bp::trace::off);
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str[] = "aaXYZbaabaXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str[] = "aaXYZbaabaXYZXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {2, 5, 10, 13, 13, 16};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 3);
    }
    {
        char const str[] = "XYZaaXYZbaabaXYZXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 5, 8, 13, 16, 16, 19};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 4);
    }
    {
        char const str[] = "XYZXYZaaXYZbaabaXYZXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 3, 6, 8, 11, 16, 19, 19, 22};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 5);
    }
    {
        char const * str = "XYZXYZaaXYZbaabaXYZXYZ";
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 3, 6, 8, 11, 16, 19, 19, 22};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 5);
    }
    {
        char const * str = "XYZXYZaaXYZbaabaXYZXYZ";
        auto const r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 3, 6, 8, 11, 16, 19, 19, 22};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin() - str, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end() - str, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 5);
    }
}

TEST(search, search_all_unicode)
{
    {
        char const str_[] = "";
        auto str = str_ | bp::as_utf8;
        auto r = bp::search_all(str, bp::lit("XYZ"), bp::ws);
        int count = 0;
        for (auto subrange : r) {
            (void)subrange;
            ++count;
        }
        EXPECT_EQ(count, 0);
    }
    {
        char const * str_ = "aaXYZb";
        auto str = str_ | bp::as_utf16;
        auto r = bp::search_all(str, bp::lit("XYZ"), bp::ws);
        int count = 0;
        int const offsets[] = {2, 5};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 1);
    }
    {
        char const str_[] = "aaXYZbaabaXYZ";
        auto str = str_ | bp::as_utf32;
        auto r = str | bp::search_all(bp::lit("XYZ"), bp::ws, bp::trace::off);
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str_[] = "aaXYZbaabaXYZ";
        auto str = str_ | bp::as_utf8;
        auto r = str | bp::search_all(bp::lit("XYZ"), bp::trace::off);
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str_[] = "aaXYZbaabaXYZ";
        auto str = str_ | bp::as_utf16;
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {2, 5, 10, 13};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 2);
    }
    {
        char const str_[] = "aaXYZbaabaXYZXYZ";
        auto str = str_ | bp::as_utf32;
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {2, 5, 10, 13, 13, 16};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 3);
    }
    {
        char const str_[] = "XYZaaXYZbaabaXYZXYZ";
        auto str = str_ | bp::as_utf8;
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 5, 8, 13, 16, 16, 19};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 4);
    }
    {
        char const str_[] = "XYZXYZaaXYZbaabaXYZXYZ";
        auto str = str_ | bp::as_utf16;
        auto r = str | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        int const offsets[] = {0, 3, 3, 6, 8, 11, 16, 19, 19, 22};
        for (auto subrange : r) {
            EXPECT_EQ(subrange.begin().base() - str_, offsets[count * 2 + 0]);
            EXPECT_EQ(subrange.end().base() - str_, offsets[count * 2 + 1]);
            ++count;
        }
        EXPECT_EQ(count, 5);
    }
}

TEST(search, doc_examples)
{
    {
        namespace bp = boost::parser;
        auto result = bp::search("aaXYZq", bp::lit("XYZ"), bp::ws);
        assert(!result.empty());
        assert(
            std::string_view(result.begin(), result.end() - result.begin()) ==
            "XYZ");
        (void)result;
    }
    {
        auto r = "XYZaaXYZbaabaXYZXYZ" | bp::search_all(bp::lit("XYZ"));
        int count = 0;
        // Prints XYZ XYZ XYZ XYZ.
        for (auto subrange : r) {
            std::cout << std::string_view(subrange.begin(), subrange.end() - subrange.begin()) << " ";
            ++count;
        }
        std::cout << "\n";
        assert(count == 4);
    }
}
