#include "regex/regex.hpp"

#include "private/character_categories.hpp"
#include "private/jit.hpp"
#include "private/nfa.hpp"

#include <memory>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>

using namespace uregex;

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

template <typename T>
constexpr auto format_list(std::string_view name, T const &list)
    -> std::string {
  std::string result;
  result += std::format("{} = [", name);
  for (auto const &group : list) {
    result += std::format("{}", group);
    if (&group != &list.back()) {
      result += ", ";
    }
  }
  result += "] ";
  return result;
}

auto unique_node_tag(Node const *node) -> std::string {
  return std::format("node_{}", (intptr_t)node);
}

auto output_subgraph(std::ostream &out_stream,
                     std::vector<std::unique_ptr<Node>> const &all_nodes,
                     std::set<Node const *> &already_graphed,
                     Node const *to_graph) -> void {
  already_graphed.insert(to_graph);
  for (auto const &edge : to_graph->edges) {
    auto const *node = all_nodes[edge.output_index].get();

    std::string label;
    if (not edge.start_groups.empty()) {
      label += format_list("start", edge.start_groups);
    }
    if (not edge.end_groups.empty()) {
      label += format_list("end", edge.end_groups);
    }
    if (not edge.counters.empty()) {
      label += format_list("counts", edge.counters);
    }

    std::print(out_stream, " {} -> {} [label=\"{}\"]\n",
               unique_node_tag(to_graph), unique_node_tag(node), label);

    if (already_graphed.find(node) == already_graphed.end()) {
      output_subgraph(out_stream, all_nodes, already_graphed, node);
    }
  }
}

auto visualize(RegexGraphImpl const &graph, std::ostream &out_stream) -> void {
  out_stream << "digraph {\n";

  for (auto const &node : graph.all_nodes) {
    auto const condition_fmt = std::visit(
        [](auto type) { return pretty_format(type); }, node->condition.type);

    std::stringstream label;
    label << condition_fmt;

    std::print(out_stream, "  {} [label=\"{}\"]\n", unique_node_tag(node.get()),
               label.str());
  }

  std::set<Node const *> already_graphed;
  output_subgraph(out_stream, graph.all_nodes, already_graphed, graph.entry);

  out_stream << "}" << std::endl;
}

} // namespace

auto RegexGraph::visualize(std::ostream &out_stream) const -> void {
  ::visualize(*impl_, out_stream);
}

auto RegexCompiled::visualize(std::ostream &out_stream) const -> void {
  ::visualize(*impl_->m_graph, out_stream);
}
