#pragma once

#include "unicode.hpp"

#include <string_view>

namespace regex::category {
struct Any {
  static constexpr std::string_view type = ".";
};

// ASCII (maybe these should just be syntax for the unicode ones)
struct ASCIIDigit {
  static constexpr std::string_view type = "d";
};

struct ASCIIWhitespace {
  static constexpr std::string_view type = "s";
};
struct ASCIIAlphaNumeric {
  static constexpr std::string_view type = "w";
};

struct Unicode {
  static constexpr std::string_view type = "p";
};

// Letters
struct Letter : Unicode {
  static constexpr std::string_view category = "L";
};
struct LetterLower : Unicode {
  static constexpr std::string_view category = "Ll";
};
struct LetterModifier : Unicode {
  static constexpr std::string_view category = "Lm";
};
struct LetterOther : Unicode {
  static constexpr std::string_view category = "Lo";
};
struct LetterTitle : Unicode {
  static constexpr std::string_view category = "Lt";
};
struct LetterUpper : Unicode {
  static constexpr std::string_view category = "Lu";
};

// Marks
struct Mark : Unicode {
  static constexpr std::string_view category = "M";
};
struct MarkSpacingCombining : Unicode {
  static constexpr std::string_view category = "Mc";
};
struct MarkEnclosing : Unicode {
  static constexpr std::string_view category = "Me";
};
struct MarkNonSpacing : Unicode {
  static constexpr std::string_view category = "Mn";
};

// Numbers
struct Number : Unicode {
  static constexpr std::string_view category = "N";
};
struct NumberDecimal : Unicode {
  static constexpr std::string_view category = "Nd";
};
struct NumberLetter : Unicode {
  static constexpr std::string_view category = "Nl";
};
struct NumberOther : Unicode {
  static constexpr std::string_view category = "No";
};

// Punctuation
struct Punctuation : Unicode {
  static constexpr std::string_view category = "P";
};
struct PunctuationConnector : Unicode {
  static constexpr std::string_view category = "Pc";
};
struct PunctuationDash : Unicode {
  static constexpr std::string_view category = "Pd";
};
struct PunctuationClose : Unicode {
  static constexpr std::string_view category = "Pe";
};
struct PunctuationFinalQuote : Unicode {
  static constexpr std::string_view category = "Pf";
};
struct PunctuationInitialQuote : Unicode {
  static constexpr std::string_view category = "Pi";
};
struct PunctuationOther : Unicode {
  static constexpr std::string_view category = "Po";
};
struct PunctuationOpen : Unicode {
  static constexpr std::string_view category = "Ps";
};

// Separators
struct Separator : Unicode {
  static constexpr std::string_view category = "Z";
};
struct SeparatorLine : Unicode {
  static constexpr std::string_view category = "Zl";
};
struct SeparatorParagraph : Unicode {
  static constexpr std::string_view category = "Zp";
};
struct SeparatorSpace : Unicode {
  static constexpr std::string_view category = "Zs";
};

// Symbols
struct Symbol : Unicode {
  static constexpr std::string_view category = "S";
};
struct SymbolCurrency : Unicode {
  static constexpr std::string_view category = "Sc";
};
struct SymbolMath : Unicode {
  static constexpr std::string_view category = "Sm";
};
struct SymbolModifier : Unicode {
  static constexpr std::string_view category = "Sk";
};
struct SymbolOther : Unicode {
  static constexpr std::string_view category = "So";
};

// Others
struct Other : Unicode {
  static constexpr std::string_view category = "C";
};
struct OtherControl : Unicode {
  static constexpr std::string_view category = "Cc";
};
struct OtherSurrogateHalf : Unicode {
  static constexpr std::string_view category = "Cs";
};
struct OtherFormat : Unicode {
  static constexpr std::string_view category = "Cf";
};
struct OtherNoncharacter : Unicode {
  static constexpr std::string_view category = "Cn";
};
struct OtherPrivateUse : Unicode {
  static constexpr std::string_view category = "Co";
};
struct OtherUnassigned : Unicode {
  static constexpr std::string_view category = "Cu";
};

using CharacterClassesList = meta::TypeList<
    ASCIIDigit, ASCIIWhitespace, ASCIIAlphaNumeric, Letter, LetterLower,
    LetterModifier, LetterOther, LetterTitle, LetterUpper, Mark,
    MarkSpacingCombining, MarkEnclosing, MarkNonSpacing, Number, NumberDecimal,
    NumberLetter, NumberOther, Punctuation, PunctuationConnector,
    PunctuationDash, PunctuationClose, PunctuationFinalQuote,
    PunctuationInitialQuote, PunctuationOther, PunctuationOpen, Separator,
    SeparatorLine, SeparatorParagraph, SeparatorSpace, Symbol, SymbolCurrency,
    SymbolMath, SymbolModifier, SymbolOther, Other, OtherControl,
    OtherSurrogateHalf, OtherFormat, OtherNoncharacter, OtherPrivateUse,
    OtherUnassigned>;

// Custom
struct Range {
  Codepoint lower;
  Codepoint upper;
};
} // namespace regex::category
