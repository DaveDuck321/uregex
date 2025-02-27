#include <cassert>
#include <cctype>
#include <cstdint>
#include <format>
#include <iostream>
#include <memory>
#include <set>
#include <stdexcept>
#include <string_view>
#include <sys/types.h>
#include <type_traits>
#include <variant>
#include <vector>

using namespace std::literals;

namespace {

template <typename... Ts> struct Overload : Ts... {
  using Ts::operator()...;
};

template <class... Ts> Overload(Ts...) -> Overload<Ts...>;

static constexpr auto ascii_toupper(std::string &input) -> void {
  for (char &character : input) {
    input = std::toupper(character);
  }
}

// https://www.rfc-editor.org/rfc/rfc9485.pdf
struct Branch;
struct Regex {
  std::vector<Branch> branches;
};

struct Codepoint {
  unsigned value;
};

struct Atom {
  struct Placeholder {};

  struct Any {
    static constexpr auto name = "."sv;
  };

  struct Unicode {
    static constexpr auto category = "p"sv;
  };

  // Letters
  struct Letter : Unicode {
    static constexpr auto name = "{L}"sv;
  };
  struct LetterLower : Unicode {
    static constexpr auto name = "{Ll}"sv;
  };
  struct LetterModifier : Unicode {
    static constexpr auto name = "{Ll}"sv;
  };
  struct LetterOther : Unicode {
    static constexpr auto name = "{Lo}"sv;
  };
  struct LetterTitle : Unicode {
    static constexpr auto name = "{Lt}"sv;
  };
  struct LetterUpper : Unicode {
    static constexpr auto name = "{Lu}"sv;
  };

  // Marks
  struct Mark : Unicode {
    static constexpr auto name = "{M}"sv;
  };
  struct MarkSpacingCombining : Unicode {
    static constexpr auto name = "{Mc}"sv;
  };
  struct MarkEnclosing : Unicode {
    static constexpr auto name = "{Me}"sv;
  };
  struct MarkNonSpacing : Unicode {
    static constexpr auto name = "{Mn}"sv;
  };

  // Numbers
  struct Number : Unicode {
    static constexpr auto name = "{N}"sv;
  };
  struct NumberDecimal : Unicode {
    static constexpr auto name = "{Nd}"sv;
  };
  struct NumberLetter : Unicode {
    static constexpr auto name = "{Nl}"sv;
  };
  struct NumberOther : Unicode {
    static constexpr auto name = "{No}"sv;
  };

  // Punctuation
  struct Punctuation : Unicode {
    static constexpr auto name = "{P}"sv;
  };
  struct PunctuationConnector : Unicode {
    static constexpr auto name = "{Pc}"sv;
  };
  struct PunctuationDash : Unicode {
    static constexpr auto name = "{Pd}"sv;
  };
  struct PunctuationClose : Unicode {
    static constexpr auto name = "{Pe}"sv;
  };
  struct PunctuationFinalQuote : Unicode {
    static constexpr auto name = "{Pf}"sv;
  };
  struct PunctuationInitialQuote : Unicode {
    static constexpr auto name = "{Pi}"sv;
  };
  struct PunctuationOther : Unicode {
    static constexpr auto name = "{Po}"sv;
  };
  struct PunctuationOpen : Unicode {
    static constexpr auto name = "{Ps}"sv;
  };

  // Separators
  struct Separator : Unicode {
    static constexpr auto name = "{Z}"sv;
  };
  struct SeparatorLine : Unicode {
    static constexpr auto name = "{Zl}"sv;
  };
  struct SeparatorParagraph : Unicode {
    static constexpr auto name = "{Zp}"sv;
  };
  struct SeparatorSpace : Unicode {
    static constexpr auto name = "{Zs}"sv;
  };

  // Symbols
  struct Symbol : Unicode {
    static constexpr auto name = "{S}"sv;
  };
  struct SymbolCurrency : Unicode {
    static constexpr auto name = "{Sc}"sv;
  };
  struct SymbolModifier : Unicode {
    static constexpr auto name = "{Sk}"sv;
  };
  struct SymbolOther : Unicode {
    static constexpr auto name = "{So}"sv;
  };

  // Others
  struct Other : Unicode {
    static constexpr auto name = "{C}"sv;
  };
  struct OtherControl : Unicode {
    static constexpr auto name = "{Cc}"sv;
  };
  struct OtherFormat : Unicode {
    static constexpr auto name = "{Cf}"sv;
  };
  struct OtherNoncharacter : Unicode {
    static constexpr auto name = "{Cn}"sv;
  };
  struct OtherPrivateUse : Unicode {
    static constexpr auto name = "{Co}"sv;
  };

  // ASCII (maybe these should just be syntax for the unicode ones)
  struct ASCIIDigit {
    static constexpr auto category = "d"sv;
  };

  struct ASCIIWhitespace {
    static constexpr auto category = "s"sv;
  };
  struct ASCIIAlphaNumeric {
    static constexpr auto category = "w"sv;
  };

  // TODO: combine negative + positive class variants into the atom variant...
  // does this make a difference?
  struct CharClass {
    using ClassVariant = std::variant<
        ASCIIDigit, ASCIIWhitespace, ASCIIAlphaNumeric, Letter, LetterLower,
        LetterModifier, LetterOther, LetterTitle, LetterUpper, Mark,
        MarkSpacingCombining, MarkEnclosing, MarkNonSpacing, Number,
        NumberDecimal, NumberLetter, NumberOther, Punctuation,
        PunctuationConnector, PunctuationDash, PunctuationClose,
        PunctuationFinalQuote, PunctuationInitialQuote, PunctuationOther,
        PunctuationOpen, Separator, SeparatorLine, SeparatorParagraph,
        SeparatorSpace, Symbol, SymbolCurrency, SymbolModifier, SymbolOther,
        Other, OtherControl, OtherFormat, OtherNoncharacter, OtherPrivateUse>;

    ClassVariant type;
    bool is_complement;
  };

  struct CharClassExpr {
    struct Range {
      Codepoint lower;
      Codepoint upper;
    };

    using CCE1 = std::variant<Range, Codepoint, CharClass>;

    std::vector<CCE1> expression;
    bool is_complement;
  };

  using AtomVariant = std::variant<Placeholder, Regex, Any, Codepoint,
                                   CharClass, CharClassExpr>;

  AtomVariant type;
};

struct Quantifier {
  struct NoneOrMore {};
  struct OneOrMore {};
  struct MaybeOne {};

  struct Range {
    unsigned lower;
    unsigned upper;
  };

  struct Exactly {
    unsigned count;
  };

  std::variant<Exactly, NoneOrMore, OneOrMore, MaybeOne, Range> type;
};

struct Piece {
  Atom atom;
  Quantifier quantifier;
};

struct Branch {
  std::vector<Piece> pieces;
};

namespace parsing {

struct Cursor {
  std::string_view text;
  size_t offset;

  constexpr auto is_at_end() const -> bool { return offset >= text.size(); }

  constexpr auto peek_or_throw(std::string_view expected) const -> char {
    if (is_at_end()) {
      throw std::runtime_error(
          std::format("Expected {} at offset {}, got EOF", expected, offset));
    }
    return peek();
  }

  constexpr auto peek() const -> char {
    assert(offset < text.size());
    return text[offset];
  }

  constexpr auto eat_next() -> void { offset += 1; }

  constexpr auto eat_or_throw(char to_eat) -> bool {
    if (try_eat(to_eat)) {
      return true;
    }

    // Failed! Throw an error
    if (offset >= text.size()) {
      throw std::runtime_error(
          std::format("Expected '{}' at offset {}, got EOF", to_eat, offset));
    }

    throw std::runtime_error(std::format("Expected '{}' at offset {}, got '{}'",
                                         to_eat, offset, peek()));
  }

  constexpr auto try_eat(char to_eat) -> bool {
    if (is_next(to_eat)) {
      eat_next();
      return true;
    }
    return false;
  }

  constexpr auto is_next(char test_char) const -> bool {
    if (offset >= text.size()) {
      return false;
    }
    return peek() == test_char;
  }

  constexpr auto is_next_or_end(char test_char) const -> bool {
    if (offset >= text.size()) {
      return true;
    }
    return peek() == test_char;
  }
};

auto parse_number(Cursor &cursor) -> unsigned {
  size_t length = 0;

  unsigned result = 0;
  while (true) {
    if (cursor.is_at_end()) {
      goto done;
    }

    char next_char = cursor.peek();
    if (next_char < '0' || next_char > '9') {
      goto done;
    }
    cursor.eat_next();

    length += 1;
    result *= 10;
    result += static_cast<unsigned>(next_char - '0');
  }

done:
  if (length == 0) {
    throw std::runtime_error(
        std::format("Expected number at offset {}", cursor.offset));
  }
  return result;
}

auto parse_regex(Cursor &cursor) -> Regex;

auto parse_utf8_char(Cursor &cursor) -> Codepoint {
  // NOTE: quick and dirty... This is not correct
  auto eat_next = [&]() -> unsigned {
    auto next_byte =
        static_cast<uint8_t>(cursor.peek_or_throw("unicode continuation"sv));
    cursor.eat_next();
    return next_byte;
  };

  unsigned first_byte = eat_next();
  if ((first_byte & 0x80U) == 0U) {
    // ASCII range
    return {first_byte};
  }

  unsigned second_byte = eat_next();
  if ((first_byte & 0xe0U) == 0xc0U) {
    // 2-bytes
    return {(second_byte & 0x3fU) + ((first_byte & 0x1fU) << 6U)};
  }

  unsigned third_byte = eat_next();
  if ((first_byte & 0xf0U) == 0xe0U) {
    // 3-bytes
    return {(third_byte & 0x3fU) + ((second_byte & 0x3fU) << 6U) +
            ((first_byte & 0x0fU) << 12U)};
  }

  unsigned fourth_byte = eat_next();
  if ((first_byte & 0xf8U) == 0xf0U) {
    // 4-bytes
    return {(fourth_byte & 0x3fU) + ((third_byte & 0x3fU) << 6U) +
            ((second_byte & 0x3fU) << 12U) + ((first_byte & 0x07U) << 18U)};
  }

  throw std::runtime_error(
      std::format("Invalid unicode encountered at offset: {}", cursor.offset));
}

auto parse_unicode_category(Cursor &cursor, bool is_complement)
    -> Atom::CharClass {
  Atom::CharClass result;
  result.is_complement = is_complement;

  cursor.eat_or_throw('{');
  switch (cursor.peek_or_throw("match group type"sv)) {
  default:
    throw std::runtime_error(
        std::format("Unexpected character category '{}' at offset '{}'",
                    cursor.peek(), cursor.offset));
  case 'L':
    // Letters
    cursor.eat_next();
    switch (cursor.peek_or_throw("letter type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Letter{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = Atom::LetterLower{};
      break;
    case 'm':
      cursor.eat_next();
      result.type = Atom::LetterModifier{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = Atom::LetterOther{};
      break;
    case 't':
      cursor.eat_next();
      result.type = Atom::LetterTitle{};
      break;
    case 'u':
      cursor.eat_next();
      result.type = Atom::LetterUpper{};
      break;
    }
    break;

  case 'M':
    // Marks
    cursor.eat_next();
    switch (cursor.peek_or_throw("mark type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Mark{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = Atom::MarkSpacingCombining{};
      break;
    case 'e':
      cursor.eat_next();
      result.type = Atom::MarkEnclosing{};
      break;
    case 'n':
      cursor.eat_next();
      result.type = Atom::MarkNonSpacing{};
      break;
    }
    break;

  case 'N':
    // Numbers
    cursor.eat_next();
    switch (cursor.peek_or_throw("number type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Number{};
      break;
    case 'd':
      cursor.eat_next();
      result.type = Atom::NumberDecimal{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = Atom::NumberLetter{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = Atom::NumberOther{};
      break;
    }
    break;

  case 'P':
    // Punctuation
    cursor.eat_next();
    switch (cursor.peek_or_throw("punctuation type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Punctuation{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = Atom::PunctuationConnector{};
      break;
    case 'd':
      cursor.eat_next();
      result.type = Atom::PunctuationDash{};
      break;
    case 'e':
      cursor.eat_next();
      result.type = Atom::PunctuationClose{};
      break;
    case 'f':
      cursor.eat_next();
      result.type = Atom::PunctuationFinalQuote{};
      break;
    case 'i':
      cursor.eat_next();
      result.type = Atom::PunctuationInitialQuote{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = Atom::PunctuationOther{};
      break;
    case 's':
      cursor.eat_next();
      result.type = Atom::PunctuationOpen{};
      break;
    }
    break;

  case 'Z':
    // Separators
    cursor.eat_next();
    switch (cursor.peek_or_throw("separator type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Separator{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = Atom::SeparatorLine{};
      break;
    case 'p':
      cursor.eat_next();
      result.type = Atom::SeparatorParagraph{};
      break;
    case 's':
      cursor.eat_next();
      result.type = Atom::SeparatorSpace{};
      break;
    }
    break;

  case 'S':
    // Symbols
    cursor.eat_next();
    switch (cursor.peek_or_throw("symbol type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Symbol{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = Atom::SymbolCurrency{};
      break;
    case 'm':
      cursor.eat_next();
      result.type = Atom::SymbolModifier{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = Atom::SymbolOther{};
      break;
    }
    break;

  case 'C':
    // Other
    cursor.eat_next();
    switch (cursor.peek_or_throw("other type or '}'"sv)) {
    default:
      throw std::runtime_error(
          std::format("Unexpected character '{}' at offset '{}'", cursor.peek(),
                      cursor.offset));
    case '}':
      result.type = Atom::Other{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = Atom::OtherControl{};
      break;
    case 'f':
      cursor.eat_next();
      result.type = Atom::OtherFormat{};
      break;
    case 'n':
      cursor.eat_next();
      result.type = Atom::OtherNoncharacter{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = Atom::OtherPrivateUse{};
      break;
    }
    break;
  }

  cursor.eat_or_throw('}');
  return result;
}

auto parse_char_or_char_class(Cursor &cursor)
    -> std::variant<Codepoint, Atom::CharClass> {
  auto is_upper_case = [](char c) { return c == std::toupper(c); };

  // Escaped character or character class
  if (cursor.try_eat('\\')) {
    char next_char = cursor.peek_or_throw("escaped group"sv);
    cursor.eat_next();
    switch (next_char) {
    default:
      throw std::runtime_error(
          std::format("Unrecognized escaped character {}", next_char));
    // Regex control characters
    case '(':
    case ')':
    case '*':
    case '+':
    case '-':
    case '.':
    case '?':
    case '[':
    case '\\':
    case ']':
    case '^':
    case '{':
    case '|':
    case '}':
      return {Codepoint{static_cast<uint8_t>(next_char)}};

    // Special characters
    case 'n':
      return {Codepoint{'\n'}};
    case 'r':
      return {Codepoint{'\r'}};
    case 't':
      return {Codepoint{'\t'}};

    // Categories
    case 'p':
    case 'P':
      return {parse_unicode_category(cursor, is_upper_case(next_char))};

    case 'd':
    case 'D':
      return {Atom::CharClass{.type = Atom::ASCIIDigit{},
                              .is_complement = is_upper_case(next_char)}};

    case 's':
    case 'S':
      return {Atom::CharClass{.type = Atom::ASCIIWhitespace{},
                              .is_complement = is_upper_case(next_char)}};

    case 'w':
    case 'W':
      return {Atom::CharClass{.type = Atom::ASCIIAlphaNumeric{},
                              .is_complement = is_upper_case(next_char)}};
    }
  }

  // Normal character
  return {parse_utf8_char(cursor)};
}

auto parse_character_class_expression(Cursor &cursor) -> Atom::CharClassExpr {
  Atom::CharClassExpr result;
  cursor.eat_or_throw('[');

  // Start
  if (cursor.try_eat('^')) {
    result.is_complement = true;
  }
  if (cursor.try_eat('-')) {
    result.expression.push_back(Codepoint{'-'});
  }

  // Body
  // Note: empty groups are not strictly allowed... But it works in Javascript
  while (not cursor.is_at_end() &&
         not(cursor.is_next('-') || cursor.is_next(']'))) {

    auto cce1 = parse_char_or_char_class(cursor);

    // Ranges must start with a standard (or escaped character)
    if (std::holds_alternative<Codepoint>(cce1)) {
      if (cursor.try_eat('-')) {
        auto next_cce1 = parse_char_or_char_class(cursor);
        if (not std::holds_alternative<Codepoint>(next_cce1)) {
          throw std::runtime_error(std::format(
              "Unexpected category in char class expression at offset {}",
              cursor.offset));
        }
        result.expression.push_back(Atom::CharClassExpr::Range{
            std::get<Codepoint>(cce1), std::get<Codepoint>(next_cce1)});
        continue;
      }
    }

    // This is a direct char/ char class match
    std::visit([&](auto value) { result.expression.push_back(value); }, cce1);
  }

  // End
  if (cursor.try_eat('-')) {
    result.expression.push_back(Codepoint{'-'});
  }
  cursor.eat_or_throw(']');
  return result;
}

auto parse_atom(Cursor &cursor) -> Atom {
  // Nested regex
  if (cursor.try_eat('(')) {
    auto result = parse_regex(cursor);
    cursor.eat_or_throw(')');
    return {{result}};
  }

  // Character class expression
  if (cursor.is_next('[')) {
    return {{parse_character_class_expression(cursor)}};
  }

  // Any '.'
  if (cursor.try_eat('.')) {
    return {Atom::Any{}};
  }

  // Normal character
  return std::visit([&](auto value) -> Atom { return {{value}}; },
                    parse_char_or_char_class(cursor));
}

auto parse_quantifier(Cursor &cursor) -> Quantifier {
  if (cursor.is_at_end()) {
    return {Quantifier::Exactly(1)};
  }
  switch (cursor.peek()) {
  default:
    return {Quantifier::Exactly(1)};
  case '*':
    cursor.eat_next();
    return {Quantifier::NoneOrMore()};
  case '+':
    cursor.eat_next();
    return {Quantifier::OneOrMore()};
  case '?':
    cursor.eat_next();
    return {Quantifier::MaybeOne()};
  case '{':
    break;
  }

  // We're parsing a numeric quantifier (either range or count)
  cursor.eat_next();

  unsigned first_number = parse_number(cursor);
  if (not cursor.try_eat(',')) {
    // Count quantifier
    cursor.eat_or_throw('}');
    return {Quantifier::Exactly{first_number}};
  }

  // Range quantifier
  unsigned second_number = parse_number(cursor);
  cursor.eat_or_throw('}');
  return {Quantifier::Range{first_number, second_number}};
}

auto parse_piece(Cursor &cursor) -> Piece {
  Piece result{
      .atom = parse_atom(cursor),
      .quantifier = parse_quantifier(cursor),
  };
  return result;
}

auto parse_branch(Cursor &cursor) -> Branch {
  Branch result;
  while (not(cursor.is_next_or_end('|') || cursor.is_next_or_end(')'))) {
    result.pieces.push_back(parse_piece(cursor));
  }
  return result;
}

auto parse_regex(Cursor &cursor) -> Regex {
  Regex result;
  do {
    result.branches.push_back(parse_branch(cursor));
  } while (cursor.try_eat('|'));
  return result;
}
} // namespace parsing

namespace matching {
struct State {
  Atom condition;
  std::set<State *> output_states;
  size_t last_added_at_index;
};

struct Fragment {
  std::set<State *> input_states;
  std::set<State *> output_states;
};

using StateList = std::vector<std::unique_ptr<matching::State>>;

auto allocate_state(StateList &all_states, Atom atom) -> State * {
  assert(not std::holds_alternative<Regex>(atom.type));

  all_states.emplace_back(new State{atom, {}, 0});
  return all_states.back().get();
}

auto build_state_tree(Regex const &regex, StateList &all_states,
                      std::set<State *> input_states) -> Fragment {

  Fragment result{};
  for (const auto &branch : regex.branches) {
    Fragment previous_fragment;
    previous_fragment.output_states = input_states;

    for (const auto &piece : branch.pieces) {
      auto get_fragment = [&]() -> Fragment {
        if (std::holds_alternative<Regex>(piece.atom.type)) {
          // Complex regex, all inputs are mapped recursively
          auto nested_regex = std::get<Regex>(piece.atom.type);
          return build_state_tree(nested_regex, all_states,
                                  previous_fragment.output_states);
        }

        // Simple comparison
        auto *state = allocate_state(all_states, {piece.atom});
        return {{state}, {state}};
      };

      Fragment fragment;

      auto merge = [&] {
        // Merge into one larger fragment
        for (auto *output_state : previous_fragment.output_states) {
          for (auto *input_state : fragment.input_states) {
            output_state->output_states.insert(input_state);
          }
        }

        previous_fragment.output_states = fragment.output_states;
      };

      std::visit(
          Overload{
              [&](Quantifier::NoneOrMore) {
                // previous -> current ->  next
                //           |----<>---|
                fragment = get_fragment();
                for (auto *output_state : fragment.output_states) {
                  for (auto *input_state : fragment.input_states) {
                    output_state->output_states.insert(input_state);
                  }
                }
                for (auto *output_state : previous_fragment.output_states) {
                  fragment.output_states.insert(output_state);
                }
                merge();
              },
              [&](Quantifier::OneOrMore) {
                // previous -> current ->  next
                //           |----<----|
                fragment = get_fragment();
                for (auto *output_state : fragment.output_states) {
                  for (auto *input_state : fragment.input_states) {
                    output_state->output_states.insert(input_state);
                  }
                }
                merge();
              },
              [&](Quantifier::MaybeOne) {
                // previous -> current ->  next
                //           |---->----|
                fragment = get_fragment();
                for (auto *output_state : previous_fragment.output_states) {
                  fragment.output_states.insert(output_state);
                }
                merge();
              },
              [&](Quantifier::Range range) {
                // previous -> n1 -> n2 -> n3 -> n4 ->  next
                //                      |-->--|-->--|
                for (size_t i = 0; i < range.lower; i += 1) {
                  fragment = get_fragment();
                  merge();
                }

                std::set<State *> additional_output_states;
                for (size_t i = range.lower; i < range.upper; i += 1) {
                  for (auto *state : previous_fragment.output_states) {
                    additional_output_states.insert(state);
                  }
                  fragment = get_fragment();
                  merge();
                }
                for (auto *state : additional_output_states) {
                  previous_fragment.output_states.insert(state);
                }
              },
              [&](Quantifier::Exactly exactly) {
                // previous -> n1 -> n2 -> nn ->  next
                for (size_t i = 0; i < exactly.count; i += 1) {
                  fragment = get_fragment();
                  merge();
                }
              }},
          piece.quantifier.type);

      if (previous_fragment.input_states.empty()) {
        previous_fragment.input_states = fragment.input_states;
      }
    }

    // Merge into the overall fragment
    for (auto *state : previous_fragment.input_states) {
      result.input_states.insert(state);
    }
    for (auto *state : previous_fragment.output_states) {
      result.output_states.insert(state);
    }
  }
  return result;
}

auto format_atom(Atom atom) -> std::string {
  return std::visit(
      Overload(
          [](Atom::Any) { return std::string{Atom::Any::name}; },
          [](Codepoint codepoint) {
            return std::format("{}", (char)codepoint.value);
          },
          [](Atom::CharClass c) {
            auto prefix = std::visit(
                [](auto c) { return std::string{c.category}; }, c.type);
            if (c.is_complement) {
              ascii_toupper(prefix);
            }

            auto name = std::visit(
                [](auto c) {
                  if constexpr (std::is_base_of_v<Atom::Unicode, decltype(c)>) {
                    return std::string{c.name};
                  } else {
                    return std::string{};
                  }
                },
                c.type);
            return std::format("\\\\{}{}", prefix, name);
          },
          [](Atom::CharClassExpr) {
            assert(!"Unreachable");
            return std::string{};
          },
          [](Atom::Placeholder) { return std::string{"Entry"}; },
          [](Regex) {
            assert(!"Unreachable");
            return std::string{};
          }),
      atom.type);
}

auto format_state(State *state) -> std::string {
  return std::format("state_{}", (intptr_t)state);
}

auto output_subgraph(std::set<State *> &already_graphed, State *to_graph)
    -> void {
  already_graphed.insert(to_graph);
  for (auto *state : to_graph->output_states) {
    std::cout << format_state(to_graph) << " -> " << format_state(state)
              << "\n";

    if (already_graphed.find(state) == already_graphed.end()) {
      output_subgraph(already_graphed, state);
    }
  }
}

auto output_graph(const StateList &all_states, State *entry_state,
                  const Fragment &fragment) -> void {

  std::cout << "digraph {\n";

  for (auto &state : all_states) {
    std::cout << std::format("{} [label=\"{}\"]\n", format_state(state.get()),
                             format_atom(state->condition));
  }

  std::set<State *> already_graphed;
  output_subgraph(already_graphed, entry_state);

  for (auto *match_state : fragment.output_states) {
    std::cout << format_state(match_state) << "-> Match" << "\n";
  }

  std::cout << "}" << std::endl;
}
} // namespace matching
} // namespace

int main(int argc, char *argv[]) {
  assert(argc == 2);
  std::string_view regex_string = argv[1];

  // Parse
  parsing::Cursor cursor{.text = regex_string, .offset = 0};
  auto regex = parsing::parse_regex(cursor);

  // Compile
  std::vector<std::unique_ptr<matching::State>> all_states;
  auto entry_state =
      matching::allocate_state(all_states, {Atom::Placeholder{}});
  auto regex_fragment =
      matching::build_state_tree(regex, all_states, {entry_state});

  // Graph
  matching::output_graph(all_states, entry_state, regex_fragment);
}
