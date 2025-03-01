#pragma once

#include "unicode.hpp"

#include <string_view>

namespace regex::category {
struct Any {
  static constexpr std::string_view category = ".";
};

// ASCII (maybe these should just be syntax for the unicode ones)
struct ASCIIDigit {
  static constexpr std::string_view category = "d";
};

struct ASCIIWhitespace {
  static constexpr std::string_view category = "s";
};
struct ASCIIAlphaNumeric {
  static constexpr std::string_view category = "w";
};

struct Unicode {
  static constexpr std::string_view category = "p";
};

// Letters
struct Letter : Unicode {
  static constexpr std::string_view subcategory = "{L}";
};
struct LetterLower : Unicode {
  static constexpr std::string_view subcategory = "{Ll}";
};
struct LetterModifier : Unicode {
  static constexpr std::string_view subcategory = "{Ll}";
};
struct LetterOther : Unicode {
  static constexpr std::string_view subcategory = "{Lo}";
};
struct LetterTitle : Unicode {
  static constexpr std::string_view subcategory = "{Lt}";
};
struct LetterUpper : Unicode {
  static constexpr std::string_view subcategory = "{Lu}";
};

// Marks
struct Mark : Unicode {
  static constexpr std::string_view subcategory = "{M}";
};
struct MarkSpacingCombining : Unicode {
  static constexpr std::string_view subcategory = "{Mc}";
};
struct MarkEnclosing : Unicode {
  static constexpr std::string_view subcategory = "{Me}";
};
struct MarkNonSpacing : Unicode {
  static constexpr std::string_view subcategory = "{Mn}";
};

// Numbers
struct Number : Unicode {
  static constexpr std::string_view subcategory = "{N}";
};
struct NumberDecimal : Unicode {
  static constexpr std::string_view subcategory = "{Nd}";
};
struct NumberLetter : Unicode {
  static constexpr std::string_view subcategory = "{Nl}";
};
struct NumberOther : Unicode {
  static constexpr std::string_view subcategory = "{No}";
};

// Punctuation
struct Punctuation : Unicode {
  static constexpr std::string_view subcategory = "{P}";
};
struct PunctuationConnector : Unicode {
  static constexpr std::string_view subcategory = "{Pc}";
};
struct PunctuationDash : Unicode {
  static constexpr std::string_view subcategory = "{Pd}";
};
struct PunctuationClose : Unicode {
  static constexpr std::string_view subcategory = "{Pe}";
};
struct PunctuationFinalQuote : Unicode {
  static constexpr std::string_view subcategory = "{Pf}";
};
struct PunctuationInitialQuote : Unicode {
  static constexpr std::string_view subcategory = "{Pi}";
};
struct PunctuationOther : Unicode {
  static constexpr std::string_view subcategory = "{Po}";
};
struct PunctuationOpen : Unicode {
  static constexpr std::string_view subcategory = "{Ps}";
};

// Separators
struct Separator : Unicode {
  static constexpr std::string_view subcategory = "{Z}";
};
struct SeparatorLine : Unicode {
  static constexpr std::string_view subcategory = "{Zl}";
};
struct SeparatorParagraph : Unicode {
  static constexpr std::string_view subcategory = "{Zp}";
};
struct SeparatorSpace : Unicode {
  static constexpr std::string_view subcategory = "{Zs}";
};

// Symbols
struct Symbol : Unicode {
  static constexpr std::string_view subcategory = "{S}";
};
struct SymbolCurrency : Unicode {
  static constexpr std::string_view subcategory = "{Sc}";
};
struct SymbolModifier : Unicode {
  static constexpr std::string_view subcategory = "{Sk}";
};
struct SymbolOther : Unicode {
  static constexpr std::string_view subcategory = "{So}";
};

// Others
struct Other : Unicode {
  static constexpr std::string_view subcategory = "{C}";
};
struct OtherControl : Unicode {
  static constexpr std::string_view subcategory = "{Cc}";
};
struct OtherFormat : Unicode {
  static constexpr std::string_view subcategory = "{Cf}";
};
struct OtherNoncharacter : Unicode {
  static constexpr std::string_view subcategory = "{Cn}";
};
struct OtherPrivateUse : Unicode {
  static constexpr std::string_view subcategory = "{Co}";
};

using CharacterClassesList = meta::TypeList<
    ASCIIDigit, ASCIIWhitespace, ASCIIAlphaNumeric, Letter, LetterLower,
    LetterModifier, LetterOther, LetterTitle, LetterUpper, Mark,
    MarkSpacingCombining, MarkEnclosing, MarkNonSpacing, Number, NumberDecimal,
    NumberLetter, NumberOther, Punctuation, PunctuationConnector,
    PunctuationDash, PunctuationClose, PunctuationFinalQuote,
    PunctuationInitialQuote, PunctuationOther, PunctuationOpen, Separator,
    SeparatorLine, SeparatorParagraph, SeparatorSpace, Symbol, SymbolCurrency,
    SymbolModifier, SymbolOther, Other, OtherControl, OtherFormat,
    OtherNoncharacter, OtherPrivateUse>;

// Custom
struct Range {
  Codepoint lower;
  Codepoint upper;
};
} // namespace regex::category
