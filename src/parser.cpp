#include "regex/parser.hpp"
#include "regex/character_categories.hpp"
#include "regex/common.hpp"

#include <cassert>
#include <cctype>
#include <set>
#include <sys/types.h>
#include <variant>
#include <vector>

using namespace regex::category;
using namespace regex;
using namespace std::string_view_literals;

namespace {
// High-level syntax
// https://www.rfc-editor.org/rfc/rfc9485.pdf
struct Branch;
struct Regex {
  std::vector<Branch> branches;
};

struct Atom {
  struct CharacterClass {
    using CharacterClassVariant =
        meta::rename<std::variant, CharacterClassesList>;

    CharacterClassVariant type;
    bool is_complement;
  };

  struct CustomClassExpression {
    using CustomExpressionItem = std::variant<Range, Codepoint, CharacterClass>;

    std::vector<CustomExpressionItem> expression;
    bool is_complement;
  };

  using AtomVariant = std::variant<Any, Codepoint, CharacterClass,
                                   CustomClassExpression, Regex>;

  AtomVariant type;
};

struct Quantifier {
  struct NoneOrMore {};
  struct OneOrMore {};

  struct Range {
    // upper = lower = N     => Exactly[N]
    // lower = 0, upper = N  => Maybe[N]
    unsigned lower;
    unsigned upper;
  };

  using QuantifierVariant = std::variant<NoneOrMore, OneOrMore, Range>;
  QuantifierVariant type;
};

struct Piece {
  Atom atom;
  Quantifier quantifier;
};

struct Branch {
  std::vector<Piece> pieces;
};

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

constexpr auto parse_number(Cursor &cursor) -> unsigned {
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
    throw ParserError("Expected number at offset {}"sv, cursor.offset);
  }
  return result;
}

constexpr auto parse_regex(Cursor &cursor) -> Regex;

constexpr auto parse_unicode_category(Cursor &cursor, bool is_complement)
    -> Atom::CharacterClass {
  Atom::CharacterClass result;
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
      result.type = category::Letter{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = category::LetterLower{};
      break;
    case 'm':
      cursor.eat_next();
      result.type = category::LetterModifier{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = category::LetterOther{};
      break;
    case 't':
      cursor.eat_next();
      result.type = category::LetterTitle{};
      break;
    case 'u':
      cursor.eat_next();
      result.type = category::LetterUpper{};
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
      result.type = category::Mark{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = category::MarkSpacingCombining{};
      break;
    case 'e':
      cursor.eat_next();
      result.type = category::MarkEnclosing{};
      break;
    case 'n':
      cursor.eat_next();
      result.type = category::MarkNonSpacing{};
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
      result.type = category::Number{};
      break;
    case 'd':
      cursor.eat_next();
      result.type = category::NumberDecimal{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = category::NumberLetter{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = category::NumberOther{};
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
      result.type = category::Punctuation{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = category::PunctuationConnector{};
      break;
    case 'd':
      cursor.eat_next();
      result.type = category::PunctuationDash{};
      break;
    case 'e':
      cursor.eat_next();
      result.type = category::PunctuationClose{};
      break;
    case 'f':
      cursor.eat_next();
      result.type = category::PunctuationFinalQuote{};
      break;
    case 'i':
      cursor.eat_next();
      result.type = category::PunctuationInitialQuote{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = category::PunctuationOther{};
      break;
    case 's':
      cursor.eat_next();
      result.type = category::PunctuationOpen{};
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
      result.type = category::Separator{};
      break;
    case 'l':
      cursor.eat_next();
      result.type = category::SeparatorLine{};
      break;
    case 'p':
      cursor.eat_next();
      result.type = category::SeparatorParagraph{};
      break;
    case 's':
      cursor.eat_next();
      result.type = category::SeparatorSpace{};
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
      result.type = category::Symbol{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = category::SymbolCurrency{};
      break;
    case 'm':
      cursor.eat_next();
      result.type = category::SymbolMath{};
      break;
    case 'k':
      cursor.eat_next();
      result.type = category::SymbolModifier{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = category::SymbolOther{};
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
      result.type = category::Other{};
      break;
    case 'c':
      cursor.eat_next();
      result.type = category::OtherControl{};
      break;
    case 's':
      // Likely not reachable with a correct utf-8 parsing implementation
      cursor.eat_next();
      result.type = category::OtherSurrogateHalf{};
      break;
    case 'f':
      cursor.eat_next();
      result.type = category::OtherFormat{};
      break;
    case 'n':
      cursor.eat_next();
      result.type = category::OtherNoncharacter{};
      break;
    case 'o':
      cursor.eat_next();
      result.type = category::OtherPrivateUse{};
      break;
    case 'u':
      cursor.eat_next();
      result.type = category::OtherUnassigned{};
      break;
    }
    break;
  }

  cursor.eat_or_throw('}');
  return result;
}

constexpr auto parse_char_or_char_class(Cursor &cursor)
    -> std::variant<Codepoint, Atom::CharacterClass> {
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
      return {Atom::CharacterClass{.type = ASCIIDigit{},
                                   .is_complement = is_upper_case(next_char)}};

    case 's':
    case 'S':
      return {Atom::CharacterClass{.type = ASCIIWhitespace{},
                                   .is_complement = is_upper_case(next_char)}};

    case 'w':
    case 'W':
      return {Atom::CharacterClass{.type = ASCIIAlphaNumeric{},
                                   .is_complement = is_upper_case(next_char)}};
    }
  }

  // Normal character
  size_t size;
  auto codepoint = parse_utf8_char(cursor.text.substr(cursor.offset), size);
  cursor.offset += size;
  return {codepoint};
}

constexpr auto parse_character_class_expression(Cursor &cursor)
    -> Atom::CustomClassExpression {
  Atom::CustomClassExpression result;
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
        result.expression.push_back(
            Range{std::get<Codepoint>(cce1), std::get<Codepoint>(next_cce1)});
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

constexpr auto parse_atom(Cursor &cursor) -> Atom {
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
    return {Any{}};
  }

  // Normal character
  return std::visit([&](auto value) -> Atom { return {{value}}; },
                    parse_char_or_char_class(cursor));
}

constexpr auto parse_quantifier(Cursor &cursor) -> Quantifier {
  if (cursor.is_at_end()) {
    return {Quantifier::Range(1, 1)};
  }
  switch (cursor.peek()) {
  default:
    return {Quantifier::Range(1, 1)};
  case '*':
    cursor.eat_next();
    return {Quantifier::NoneOrMore()};
  case '+':
    cursor.eat_next();
    return {Quantifier::OneOrMore()};
  case '?':
    cursor.eat_next();
    return {Quantifier::Range(0, 1)};
  case '{':
    break;
  }

  // We're parsing a numeric quantifier (either range or count)
  cursor.eat_next();

  unsigned first_number = parse_number(cursor);
  if (not cursor.try_eat(',')) {
    // Count quantifier
    cursor.eat_or_throw('}');
    return {Quantifier::Range{first_number, first_number}};
  }

  // Range quantifier
  unsigned second_number = parse_number(cursor);
  cursor.eat_or_throw('}');
  return {Quantifier::Range{first_number, second_number}};
}

constexpr auto parse_piece(Cursor &cursor) -> Piece {
  return {
      .atom = parse_atom(cursor),
      .quantifier = parse_quantifier(cursor),
  };
}

constexpr auto parse_branch(Cursor &cursor) -> Branch {
  Branch result;
  while (not(cursor.is_next_or_end('|') || cursor.is_next_or_end(')'))) {
    result.pieces.push_back(parse_piece(cursor));
  }
  return result;
}

constexpr auto parse_regex(Cursor &cursor) -> Regex {
  Regex result;
  do {
    result.branches.push_back(parse_branch(cursor));
  } while (cursor.try_eat('|'));
  return result;
}

// Node tree building
struct Fragment {
  std::set<Node *> input_nodes;
  std::set<Node *> output_nodes;
};

template <typename T>
constexpr auto atom_class_to(Atom::CharacterClass input) -> T {
  return std::visit(
      [&]<typename Class>(Class) -> T {
        return {Condition::CharacterClass<Class>{input.is_complement}};
      },
      input.type);
}

constexpr auto allocate_node(std::vector<std::unique_ptr<Node>> &all_nodes,
                             Condition condition) -> Node * {
  all_nodes.emplace_back(new Node{all_nodes.size(), condition, {}, {}, {}});
  return all_nodes.back().get();
}

constexpr auto allocate_node(std::vector<std::unique_ptr<Node>> &all_nodes,
                             Atom atom) -> Node * {
  using ExpressionVariant =
      Condition::CustomExpression::CustomExpressionVariant;

  auto condition = std::visit(
      Overload{
          [&](Any any) { return Condition{any}; },
          [&](Codepoint codepoint) { return Condition{codepoint}; },
          [&](Atom::CharacterClass character_class) {
            return atom_class_to<Condition>(character_class);
          },
          [&](Atom::CustomClassExpression expr) {
            Condition::CustomExpression condition;
            condition.is_complement = expr.is_complement;
            for (const auto &expression : expr.expression) {
              ExpressionVariant expression_variant = std::visit(
                  Overload{
                      [&](Codepoint cp) { return ExpressionVariant{cp}; },
                      [&](Range range) { return ExpressionVariant{range}; },
                      [&](Atom::CharacterClass char_class) {
                        return atom_class_to<ExpressionVariant>(char_class);
                      },
                  },
                  expression);
              condition.expressions.push_back(std::move(expression_variant));
            }
            return Condition{condition};
          },
          [&](Regex) -> Condition { assert(!"Unreachable"); }},
      atom.type);

  return allocate_node(all_nodes, condition);
}

auto build_fragment(Regex const &regex,
                    std::vector<std::unique_ptr<Node>> &all_nodes,
                    std::set<Node *> input_nodes, size_t &current_group_index)
    -> Fragment {
  Fragment result{};
  for (const auto &branch : regex.branches) {
    Fragment previous_fragment;
    previous_fragment.output_nodes = input_nodes;

    for (const auto &piece : branch.pieces) {
      std::optional<size_t> allocated_group = {};

      auto get_fragment = [&]() -> Fragment {
        if (std::holds_alternative<Regex>(piece.atom.type)) {
          // Complex regex, all inputs are mapped recursively
          if (not allocated_group.has_value()) {
            allocated_group = current_group_index++;
          }

          auto nested_regex = std::get<Regex>(piece.atom.type);
          auto fragment = build_fragment(nested_regex, all_nodes,
                                         previous_fragment.output_nodes,
                                         current_group_index);
          for (auto *node : fragment.input_nodes) {
            node->start_of_groups.insert(*allocated_group);
          }
          for (auto *node : fragment.output_nodes) {
            node->end_of_groups.insert(*allocated_group);
          }
          return fragment;
        }

        // Simple comparison
        auto *node = allocate_node(all_nodes, {piece.atom});
        return {{node}, {node}};
      };

      Fragment fragment;

      auto merge = [&] {
        // Merge into one larger fragment
        for (auto *output_node : previous_fragment.output_nodes) {
          for (auto *input_node : fragment.input_nodes) {
            output_node->output_nodes.insert(input_node);
          }
        }

        previous_fragment.output_nodes = fragment.output_nodes;
      };

      std::visit(
          Overload{[&](Quantifier::NoneOrMore) {
                     // previous -> current ->  next
                     //           |----<>---|
                     fragment = get_fragment();
                     for (auto *output_node : fragment.output_nodes) {
                       for (auto *input_node : fragment.input_nodes) {
                         output_node->output_nodes.insert(input_node);
                       }
                     }
                     for (auto *output_node : previous_fragment.output_nodes) {
                       fragment.output_nodes.insert(output_node);
                     }
                     merge();
                   },
                   [&](Quantifier::OneOrMore) {
                     // previous -> current ->  next
                     //           |----<----|
                     fragment = get_fragment();
                     for (auto *output_node : fragment.output_nodes) {
                       for (auto *input_node : fragment.input_nodes) {
                         output_node->output_nodes.insert(input_node);
                       }
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

                     std::set<Node *> additional_output_nodes;
                     for (size_t i = range.lower; i < range.upper; i += 1) {
                       for (auto *node : previous_fragment.output_nodes) {
                         additional_output_nodes.insert(node);
                       }
                       fragment = get_fragment();
                       merge();
                     }
                     for (auto *node : additional_output_nodes) {
                       previous_fragment.output_nodes.insert(node);
                     }
                   }},
          piece.quantifier.type);

      if (previous_fragment.input_nodes.empty()) {
        previous_fragment.input_nodes = fragment.input_nodes;
      }
    }

    // Merge into the overall fragment
    for (auto *node : previous_fragment.input_nodes) {
      result.input_nodes.insert(node);
    }
    for (auto *node : previous_fragment.output_nodes) {
      result.output_nodes.insert(node);
    }
  }
  return result;
}
} // namespace

auto regex::parse(std::string_view regex_string) -> RegexGraph {
  // Parse into AST
  Cursor cursor{.text = regex_string, .offset = 0};
  auto regex = parse_regex(cursor);

  // Convert into node graph
  std::vector<std::unique_ptr<Node>> all_nodes;
  size_t number_of_groups = 0;
  auto *entry_node = allocate_node(all_nodes, {Condition::Entry{}});
  auto regex_fragment =
      build_fragment(regex, all_nodes, {entry_node}, number_of_groups);
  auto *match_node = allocate_node(all_nodes, {Condition::Match{}});
  for (auto *final_node : regex_fragment.output_nodes) {
    final_node->output_nodes.insert(match_node);
  }

  return {
      .all_nodes = std::move(all_nodes),
      .entry = entry_node,
      .match = match_node,
      .number_of_groups = number_of_groups,
  };
}
