#pragma once

namespace uregex::meta {
template <typename... Ts> struct Overload : Ts... {
  using Ts::operator()...;
};
template <typename... Ts> Overload(Ts...) -> Overload<Ts...>;

template <typename... Ts> struct TypeList {};

namespace impl {
template <template <typename...> typename TargetTemplate, typename ListType>
struct Rename;
template <template <typename...> typename TargetTemplate,
          template <typename...> typename InputTemplate, typename... InputTypes>
struct Rename<TargetTemplate, InputTemplate<InputTypes...>> {
  using type = TargetTemplate<InputTypes...>;
};

template <typename ListType1, typename ListType2> struct Concat;
template <template <typename...> typename TypeList1, typename... Types1,
          template <typename...> typename TypeList2, typename... Types2>
struct Concat<TypeList1<Types1...>, TypeList2<Types2...>> {
  using type = TypeList<Types1..., Types2...>;
};

template <template <typename> typename TemplateFn, typename ListType>
struct Apply;
template <template <typename> typename TemplateFn,
          template <typename...> typename ListTemplate, typename... ListItem>
struct Apply<TemplateFn, ListTemplate<ListItem...>> {
  using type = TypeList<TemplateFn<ListItem>...>;
};

template <typename ToTest> struct EmptyDetector : ToTest {
  using Member = char;
  Member _;

  static constexpr auto is_empty() -> bool {
    return sizeof(EmptyDetector) == sizeof(Member);
  }
};
} // namespace impl

template <template <typename...> typename TargetTemplate, typename ListType>
using rename = impl::Rename<TargetTemplate, ListType>::type;

template <typename ListType1, typename ListType2>
using concat = impl::Concat<ListType1, ListType2>::type;

template <template <typename> typename TemplateFn, typename ListType>
using apply = impl::Apply<TemplateFn, ListType>::type;

template <typename T>
static constexpr auto is_empty = impl::EmptyDetector<T>::is_empty();
} // namespace uregex::meta
