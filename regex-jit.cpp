#include <cassert>
#include <cstdint>
#include <format>
#include <stdexcept>
#include <string_view>
#include <sys/types.h>
#include <variant>
#include <vector>

using namespace std::literals;

namespace {

// https://www.rfc-editor.org/rfc/rfc9485.pdf
struct Branch;
struct Regex {
  std::vector<Branch> branches;
};

struct Codepoint {
  unsigned value;
};

struct Atom {
  struct Any {};

  // Letters
  struct Letter {};         // L
  struct LetterLower {};    // Ll
  struct LetterModifier {}; // Lm
  struct LetterOther {};    // Lo
  struct LetterTitle {};    // Lt
  struct LetterUpper {};    // Lu

  // Marks
  struct Mark {};                 // M
  struct MarkSpacingCombining {}; // Mc
  struct MarkEnclosing {};        // Me
  struct MarkNonSpacing {};       // Mn

  // Numbers
  struct Number {};        // N
  struct NumberDecimal {}; // Nd
  struct NumberLetter {};  // Nl
  struct NumberOther {};   // No

  // Punctuation
  struct Punctuation {};             // P
  struct PunctuationConnector {};    // Pc
  struct PunctuationDash {};         // Pd
  struct PunctuationClose {};        // Pe
  struct PunctuationFinalQuote {};   // Pf
  struct PunctuationInitialQuote {}; // Pi
  struct PunctuationOther {};        // Po
  struct PunctuationOpen {};         // Ps

  // Separators
  struct Separator {};          // Z
  struct SeparatorLine {};      // Zl
  struct SeparatorParagraph {}; // Zp
  struct SeparatorSpace {};     // Zs

  // Symbols
  struct Symbol {};         // S
  struct SymbolCurrency {}; // Sc
  struct SymbolModifier {}; // Sk
  struct SymbolOther {};    // So

  // Others
  struct Other {};             // C
  struct OtherControl {};      // Cc
  struct OtherFormat {};       // Cf
  struct OtherNoncharacter {}; // Cn
  struct OtherPrivateUse {};   // Co

  // Classes

  // TODO: combine negative + positive class variants into the atom variant...
  // does this make a difference?
  struct CharClass {
    using ClassVariant = std::variant<
        Letter, LetterLower, LetterModifier, LetterOther, LetterTitle,
        LetterUpper, Mark, MarkSpacingCombining, MarkEnclosing, MarkNonSpacing,
        Number, NumberDecimal, NumberLetter, NumberOther, Punctuation,
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

  using AtomVariant =
      std::variant<Regex, Any, Codepoint, CharClass, CharClassExpr>;

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
          std::format("Expected '}}' at offset {}, got EOF", offset));
    }

    throw std::runtime_error(
        std::format("Expected '}}' at offset {}, got '{}'", offset, peek()));
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

auto parse_character_category(Cursor &cursor) -> Atom::CharClass {
  Atom::CharClass result;
  result.is_complement = (cursor.peek() == 'P');

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
  // Escaped character or character class
  if (cursor.try_eat('\\')) {
    char next_char = cursor.peek_or_throw("escaped group"sv);
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
      cursor.eat_next();
      return {Codepoint{'\n'}};
    case 'r':
      cursor.eat_next();
      return {Codepoint{'\r'}};
    case 't':
      cursor.eat_next();
      return {Codepoint{'\t'}};

    // Categories
    case 'p':
    case 'P':
      return {parse_character_category(cursor)};
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

  // Normal character
  return std::visit([&](auto value) -> Atom { return {{value}}; },
                    parse_char_or_char_class(cursor));
}

auto parse_quantifier(Cursor &cursor) -> Quantifier {
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
  while (cursor.is_next_or_end('|')) {
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
} // namespace
