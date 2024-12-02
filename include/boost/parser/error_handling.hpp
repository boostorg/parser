#ifndef BOOST_PARSER_ERROR_HANDLING_HPP
#define BOOST_PARSER_ERROR_HANDLING_HPP

#include <boost/parser/error_handling_fwd.hpp>
#include <boost/parser/detail/printing.hpp>
#ifdef _MSC_VER
#include <boost/parser/detail/vs_output_stream.hpp>
#endif

#include <boost/parser/detail/text/algorithm.hpp>
#include <boost/parser/detail/text/transcode_iterator.hpp>

#include <algorithm>
#include <array>
#include <functional>
#include <iostream>
#include <sstream>


namespace boost { namespace parser {

    namespace detail {

        // All the hard line break code points from the Unicode Line Break
        // Algorithm; see https://unicode.org/reports/tr14.
        inline constexpr std::array<int, 7> eol_cps = {
            {0x000a, 0x000b, 0x000c, 0x000d, 0x0085, 0x2028, 0x2029}};

        inline constexpr int eol_cp_mask =
            0x000a | 0x000b | 0x000c | 0x000d | 0x0085 | 0x2028 | 0x2029;
    }

    /** Returns the `line_position` for `it`, counting lines from the
        beginning of the input `first`. */
    template<typename Iter>
    line_position<Iter> find_line_position(Iter first, Iter it)
    {
        bool prev_cr = false;
        auto retval = line_position<Iter>{first, 0, 0};
        for (Iter pos = first; pos != it; ++pos) {
            auto const c = *pos;
            bool const found =
                (c & detail::eol_cp_mask) == c &&
                std::find(detail::eol_cps.begin(), detail::eol_cps.end(), c) !=
                    detail::eol_cps.end();
            if (found) {
                retval.line_start = std::next(pos);
                retval.column_number = 0;
            } else {
                ++retval.column_number;
            }
            if (found && (!prev_cr || c != 0x000a))
                ++retval.line_number;
            prev_cr = c == 0x000d;
        }
        return retval;
    }

    /** Returns the iterator to the end of the line in which `it` is
        found.  */
    template<typename Iter, typename Sentinel>
    Iter find_line_end(Iter it, Sentinel last)
    {
        return parser::detail::text::find_if(it, last, [](auto c) {
            return (c & detail::eol_cp_mask) == c &&
                   std::find(
                       detail::eol_cps.begin(), detail::eol_cps.end(), c) !=
                       detail::eol_cps.end();
        });
    }

    template<typename Iter, typename Sentinel>
    std::ostream & write_formatted_message(
        std::ostream & os,
        std::string_view filename,
        Iter first_,
        Iter it_,
        Sentinel last_,
        std::string_view message,
        int64_t preferred_max_line_length,
        int64_t max_after_caret)
    {
        auto [first, it, last] =
            parser::normalize_iterators(first_, it_, last_);

        if (!filename.empty())
            os << filename << ':';
        auto const position = parser::find_line_position(first, it);
        os << (position.line_number + 1) << ':' << position.column_number
           << ": " << message << " here";
        if (it == last)
            os << " (end of input)";
        os << ":\n";

        std::string underlining(std::distance(position.line_start, it), ' ');
        detail::trace_input(os, position.line_start, it, false, 1u << 31);
        if (it == last) {
            os << '\n' << underlining << "^\n";
            os.rdbuf()->pubsync();
            return os;
        }

        underlining += '^';

        int64_t const limit = (std::max)(
            preferred_max_line_length,
            (int64_t)underlining.size() + max_after_caret);

        int64_t i = (int64_t)underlining.size();
        auto const line_end = parser::find_line_end(std::next(it), last);
        detail::trace_input(os, it, line_end, false, limit - i);

        os << '\n' << underlining << '\n';
        os.rdbuf()->pubsync();

        return os;
    }

#if defined(_MSC_VER)
    template<typename Iter, typename Sentinel>
    std::ostream & write_formatted_message(
        std::ostream & os,
        std::wstring_view filename,
        Iter first_,
        Iter it_,
        Sentinel last_,
        std::string_view message,
        int64_t preferred_max_line_length,
        int64_t max_after_caret)
    {
        auto [first, it, last] =
            parser::normalize_iterators(first_, it_, last_);
        auto const r = filename | parser::detail::text::as_utf8;
        std::string s(r.begin(), r.end());
        return parser::write_formatted_message(
            os,
            s,
            first,
            it,
            last,
            message,
            preferred_max_line_length,
            max_after_caret);
    }
#endif

    template<typename Iter, typename Sentinel, template<class> class Exception>
    std::ostream & write_formatted_expectation_failure_error_message(
        std::ostream & os,
        std::string_view filename,
        Iter first_,
        Sentinel last_,
        Exception<Iter> const & e,
        int64_t preferred_max_line_length,
        int64_t max_after_caret)
    {
        std::string message = "error: Expected ";
        message += e.what();
        // TODO: Document that this gracefully handles token iterators, and
        // document the other parts of the API that do or do not.
        auto [first, it, last] = parser::normalize_iterators(first_, e, last_);
        return parser::write_formatted_message(
            os,
            filename,
            first,
            it,
            last,
            message,
            preferred_max_line_length,
            max_after_caret);
    }

#if defined(_MSC_VER)
    template<typename Iter, typename Sentinel, template<class> class Exception>
    std::ostream & write_formatted_expectation_failure_error_message(
        std::ostream & os,
        std::wstring_view filename,
        Iter first,
        Sentinel last,
        Exception<Iter> const & e,
        int64_t preferred_max_line_length,
        int64_t max_after_caret)
    {
        auto const r = filename | parser::detail::text::as_utf8;
        std::string s(r.begin(), r.end());
        return parser::write_formatted_expectation_failure_error_message(
            os, s, first, last, e, preferred_max_line_length, max_after_caret);
    }
#endif

    namespace detail {
        template<typename I, typename S>
        auto normalize_iterators_impl(I first, I it, S last)
        {
            if constexpr (detail::is_token_iter_v<I>)
                return std::tuple(it.range_begin(), it.base(), it.range_end());
            else
                return std::tuple(first, it, last);
        }
    }

    template<typename I, typename S>
    auto normalize_iterators(I first, I it, S last)
    {
        return detail::normalize_iterators_impl(first, it, last);
    }

    template<typename I, typename S>
    auto normalize_iterators(I first, parse_error<I> e, S last)
    {
        return detail::normalize_iterators_impl(first, e.iter, last);
    }

    template<typename I, typename S>
    auto normalize_iterators(I first, lex_error<I> e, S last)
    {
        return detail::normalize_iterators_impl(first, e.iter, last);
    }

    /** An error handler that allows users to supply callbacks to handle the
        reporting of warnings and errors.  The reporting of errors and/or
        warnings can be suppressed by supplying one or both
        default-constructed callbacks. */
    struct callback_error_handler
    {
        using callback_type = std::function<void(std::string const &)>;

        callback_error_handler() {}
        callback_error_handler(
            callback_type error, callback_type warning = callback_type()) :
            error_(error), warning_(warning)
        {}
        callback_error_handler(
            std::string_view filename,
            callback_type error,
            callback_type warning = callback_type()) :
            error_(error), warning_(warning), filename_(filename)
        {}
#if defined(_MSC_VER) || defined(BOOST_PARSER_DOXYGEN)
        /** This overload is Windows-only. */
        callback_error_handler(
            std::wstring_view filename,
            callback_type error,
            callback_type warning = callback_type()) :
            error_(error), warning_(warning)
        {
            auto const r = filename | parser::detail::text::as_utf8;
            filename_.assign(r.begin(), r.end());
        }
#endif
        template<
            typename Iter,
            typename Sentinel,
            template<class>
            class Exception>
        error_handler_result
        operator()(Iter first, Sentinel last, Exception<Iter> const & e) const
        {
            if (error_) {
                std::stringstream ss;
                parser::write_formatted_expectation_failure_error_message(
                    ss, filename_, first, last, e);
                error_(ss.str());
            }
            return error_handler_result::fail;
        }

        // TODO: Add a test that exercises this function when doing token
        // parsing.
        template<typename Context, typename Iter>
        void diagnose(
            diagnostic_kind kind,
            std::string_view message,
            Context const & context,
            Iter it) const
        {
            callback_type const & cb =
                kind == diagnostic_kind::error ? error_ : warning_;
            if (!cb)
                return;
            std::stringstream ss;
            parser::write_formatted_message(
                ss,
                filename_,
                parser::_begin(context),
                it,
                parser::_end(context),
                message);
            cb(ss.str());
        }

        template<typename Context>
        void diagnose(
            diagnostic_kind kind,
            std::string_view message,
            Context const & context) const
        {
            diagnose(kind, message, context, parser::_where(context).begin());
        }

        callback_type error_;
        callback_type warning_;
        std::string filename_;
    };

    struct rethrow_error_handler
    {
        template<
            typename Iter,
            typename Sentinel,
            template<class>
            class Exception>
        error_handler_result
        operator()(Iter first, Sentinel last, Exception<Iter> const & e) const
        {
            return error_handler_result::rethrow;
        }

        template<typename Context, typename Iter>
        void diagnose(
            diagnostic_kind kind,
            std::string_view message,
            Context const & context,
            Iter it) const
        {}

        template<typename Context>
        void diagnose(
            diagnostic_kind kind,
            std::string_view message,
            Context const & context) const
        {}
    };

#if defined(_MSC_VER) || defined(BOOST_PARSER_DOXYGEN)
    struct vs_output_error_handler : stream_error_handler
    {
        vs_output_error_handler() :
            stream_error_handler{"", detail::vs_cout, detail::vs_cout}
        {}

        vs_output_error_handler(std::string_view filename) :
            stream_error_handler{filename, detail::vs_cout, detail::vs_cout}
        {}

        vs_output_error_handler(std::wstring_view filename) :
            stream_error_handler{filename, detail::vs_cout, detail::vs_cout}
        {}
    };
#endif


    // implementations

    template<typename Iter, typename Sentinel, template<class> class Exception>
    error_handler_result default_error_handler::operator()(
        Iter first, Sentinel last, Exception<Iter> const & e) const
    {
        parser::write_formatted_expectation_failure_error_message(
            std::cerr, "", first, last, e);
        return error_handler_result::fail;
    }

    template<typename Context, typename Iter>
    void default_error_handler::diagnose(
        diagnostic_kind kind,
        std::string_view message,
        Context const & context,
        Iter it) const
    {
        parser::write_formatted_message(
            std::cerr,
            "",
            parser::_begin(context),
            it,
            parser::_end(context),
            message);
    }

    template<typename Context>
    void default_error_handler::diagnose(
        diagnostic_kind kind,
        std::string_view message,
        Context const & context) const
    {
        diagnose(kind, message, context, parser::_where(context).begin());
    }

    template<typename Iter, typename Sentinel, template<class> class Exception>
    error_handler_result stream_error_handler::operator()(
        Iter first, Sentinel last, Exception<Iter> const & e) const
    {
        std::ostream * os = err_os_;
        if (!os)
            os = &std::cerr;
        parser::write_formatted_expectation_failure_error_message(
            *os, filename_, first, last, e);
        return error_handler_result::fail;
    }

    template<typename Context, typename Iter>
    void stream_error_handler::diagnose(
        diagnostic_kind kind,
        std::string_view message,
        Context const & context,
        Iter it) const
    {
        std::ostream * os = kind == diagnostic_kind::error ? err_os_ : warn_os_;
        if (!os)
            os = &std::cerr;
        parser::write_formatted_message(
            *os,
            filename_,
            parser::_begin(context),
            it,
            parser::_end(context),
            message);
    }

    template<typename Context>
    void stream_error_handler::diagnose(
        diagnostic_kind kind,
        std::string_view message,
        Context const & context) const
    {
        diagnose(kind, message, context, parser::_where(context).begin());
    }

}}

#endif
