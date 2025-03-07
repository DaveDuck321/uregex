#include "regex/visualize.hpp"
#include "regex/character_categories.hpp"
#include "regex/nfa.hpp"

#include <set>
#include <sstream>
#include <string>
#include <type_traits>

using namespace regex;

namespace {
constexpr auto to_uppercase(std::string &input) -> void {
  for (char &c : input) {
    c = std::toupper(c);
  }
}

constexpr auto pretty_format(Condition::Entry) -> std::string {
  return "entry";
}

constexpr auto pretty_format(Condition::Match) -> std::string {
  return "match";
}

constexpr auto pretty_format(category::Any) -> std::string {
  return std::string{category::Any::type};
}

template <typename Class>
constexpr auto pretty_format(Condition::CharacterClass<Class> character_class)
    -> std::string {

  std::string type = std::string{Class::type};
  if (character_class.is_complement) {
    to_uppercase(type);
  }

  if constexpr (std::is_base_of_v<category::Unicode, Class>) {
    return std::format("\\\\{}{{{}}}", type, Class::category);
  } else {
    return std::format("\\\\{}", type);
  }
}

constexpr auto pretty_format(Codepoint codepoint) -> std::string {
  std::string output;
  codepoint_to_utf8(output, codepoint);
  return output;
}

constexpr auto pretty_format(category::Range range) -> std::string {
  std::string output;
  codepoint_to_utf8(output, range.lower);
  output += "-";
  codepoint_to_utf8(output, range.upper);
  return output;
}

constexpr auto pretty_format(Condition::CustomExpression expression)
    -> std::string {
  std::string output;
  output += "[";
  if (expression.is_complement) {
    output += "^";
  }

  for (auto const &condition : expression.expressions) {
    output +=
        std::visit([](auto type) { return pretty_format(type); }, condition);
  }
  output += "]";
  return output;
}

auto unique_node_tag(Node const *node) -> std::string {
  return std::format("node_{}", (intptr_t)node);
}

auto output_subgraph(std::ostream &out_stream,
                     std::set<Node const *> &already_graphed,
                     Node const *to_graph) -> void {
  already_graphed.insert(to_graph);
  for (auto const *node : to_graph->output_nodes) {
    out_stream << "  " << unique_node_tag(to_graph) << " -> "
               << unique_node_tag(node) << "\n";

    if (already_graphed.find(node) == already_graphed.end()) {
      output_subgraph(out_stream, already_graphed, node);
    }
  }
}
} // namespace

auto regex::output_graph(std::ostream &out_stream, const RegexGraph &graph)
    -> void {
  out_stream << "digraph {\n";

  for (auto const &node : graph.all_nodes) {
    auto const condition_fmt = std::visit(
        [](auto type) { return pretty_format(type); }, node->condition.type);

    std::stringstream label;
    label << condition_fmt;

    if (!node->start_of_groups.empty()) {
      label << ", [start=";
      for (auto const &group : node->start_of_groups) {
        label << group;
        if (&group != &node->start_of_groups.back()) {
          label << ", ";
        }
      }
      label << "]";
    }
    if (!node->end_of_groups.empty()) {
      label << ", [end=";
      for (auto const &group : node->end_of_groups) {
        label << group;
        if (&group != &node->end_of_groups.back()) {
          label << ", ";
        }
      }
      label << "]";
    }

    out_stream << std::format("  {} [label=\"{}\"]\n",
                              unique_node_tag(node.get()), label.str());
  }

  std::set<Node const *> already_graphed;
  output_subgraph(out_stream, already_graphed, graph.entry);

  out_stream << "}" << std::endl;
}
