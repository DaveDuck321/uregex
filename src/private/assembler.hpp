#pragma once

#include "private/jit.hpp"

#include <array>
#include <cassert>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <sys/mman.h>
#include <unistd.h>
#include <utility>
#include <vector>

namespace uregex::jit {
using IndexInProgram = int64_t;

enum class Register : uint8_t {
  rax = 0b000,
  rcx = 0b001,
  rdx = 0b010,
  rbx = 0b011,
  rsp = 0b100,
  rbp = 0b101,
  rsi = 0b110,
  rdi = 0b111,

  r8 = 0b1000,
  r9 = 0b1001,
  r10 = 0b1010,
  r11 = 0b1011,
  r12 = 0b1100,
  r13 = 0b1101,
  r14 = 0b1110,
  r15 = 0b1111,
};

struct CallingConvention {
  static constexpr auto ret = Register::rax;

  // Also caller saved
  static constexpr std::array argument = {Register::rdi, Register::rsi,
                                          Register::rdx, Register::rcx,
                                          Register::r8,  Register::r9};

  static constexpr std::array temporary = {
      Register::rax, Register::r10, Register::rdi, Register::rsi,
      Register::rdx, Register::rcx, Register::r8,  Register::r9};

  // Callee saved registers that we might want to clobber
  static constexpr std::array callee_saved = {Register::rbx, Register::rbp,
                                              Register::r12, Register::r13,
                                              Register::r14, Register::r15};
};

struct Label {
  enum class LabelId : size_t {};
  LabelId id;

  auto operator<(Label const &other) const -> bool {
    return std::to_underlying(id) < std::to_underlying(other.id);
  }
};

struct Fixup {
  enum class Type {
    relative_4_bytes,
  };
  IndexInProgram location;
  Type type;
};

struct Program {
  std::vector<uint8_t> data;

  constexpr auto insert_padding(size_t align_to_boundary) -> void {
    if (data.size() % align_to_boundary == 0) {
      return;
    }
    auto const bytes_to_insert =
        align_to_boundary - (data.size() % align_to_boundary);
    for (size_t i = 0; i < bytes_to_insert; i += 1) {
      data.push_back(/*nop*/ 0x90);
    }
  }

  constexpr auto insert_immediate(std::integral auto imm) -> void {
    auto immediate = std::bit_cast<std::array<uint8_t, sizeof(imm)>>(imm);
    insert_bytes(immediate);
  }

  constexpr auto insert_byte(uint8_t byte) -> void { data.push_back(byte); }

  constexpr auto insert_bytes(std::span<uint8_t const> bytes) -> void {
    for (auto byte : bytes) {
      insert_byte(byte);
    }
  }

  constexpr auto current_offset() -> IndexInProgram { return data.size(); }
};

struct Assembler {
  Program program;

  std::map<Label, std::vector<Fixup>> label_to_fixups;
  std::map<Label, IndexInProgram> label_to_location;
  std::vector<Label> labels;

  constexpr auto allocate_label() -> Label {
    auto const label = Label{Label::LabelId{labels.size()}};
    labels.push_back(label);
    return label;
  }

  template <typename Builder>
  constexpr auto build_function(Label fn_label, Builder &&fn_builder) -> void;

  template <typename Builder>
  constexpr auto build_out_of_line_block(Label fn_label, Builder &&fn_builder)
      -> void;

  auto apply_all_fixups() -> void {
    for (auto const &[label, fixups] : label_to_fixups) {
      auto const dest_location = label_to_location.at(label);
      for (auto fixup : fixups) {
        switch (fixup.type) {
        case Fixup::Type::relative_4_bytes: {
          int32_t const offset = dest_location - (fixup.location + 4);
          auto to_write = std::bit_cast<std::array<uint8_t, 4>>(offset);
          assert(program.data[fixup.location] == 0 &&
                 program.data[fixup.location + 1] == 0 &&
                 program.data[fixup.location + 2] == 0 &&
                 program.data[fixup.location + 3] == 0);

          program.data[fixup.location] = to_write[0];
          program.data[fixup.location + 1] = to_write[1];
          program.data[fixup.location + 2] = to_write[2];
          program.data[fixup.location + 3] = to_write[3];
          break;
        }
        }
      }
    }
    label_to_fixups.clear();
  }

  auto load() -> jit::ExecutableSection {
    return jit::ExecutableSection{program.data};
  }
};

class FunctionBuilder {
  Assembler *assembler;
  Program *program;
  std::vector<Register> saved;
  bool requires_return;

  constexpr auto maybe_insert_rex(std::optional<Register> other_reg = {},
                                  std::optional<Register> rm_reg = {},
                                  bool is_u8 = false) -> void {
    auto reg_encoding = std::to_underlying(rm_reg.value_or(Register::rax));
    auto other_reg_encoding =
        std::to_underlying(other_reg.value_or(Register::rax));
    if ((reg_encoding <= 0b111 && other_reg_encoding <= 0b111) && !is_u8) {
      return;
    }
    if ((reg_encoding <= 0b011 && other_reg_encoding <= 0b011) && is_u8) {
      return;
    }
    insert_rex(/*is_operand=*/false, other_reg, rm_reg);
  }

  constexpr auto insert_rex(bool is_operand = false,
                            std::optional<Register> other_reg = {},
                            std::optional<Register> rm_reg = {}) -> void {
    auto reg_encoding = std::to_underlying(rm_reg.value_or(Register::rax));
    auto other_reg_encoding =
        std::to_underlying(other_reg.value_or(Register::rax));

    uint8_t rex = 0b1000000;
    if (is_operand) {
      rex |= (1U << 3U);
    }
    if (reg_encoding > 0b111) {
      rex |= (1U << 2U);
    }
    if (other_reg_encoding > 0b111) {
      rex |= 1U;
    }
    program->insert_byte(rex);
  }

  constexpr auto insert_modrm(Register reg, unsigned mod, Register rm) -> void {
    assert(mod <= 0b11);

    uint8_t value = 0;
    value |= (std::to_underlying(rm) & 0b111);
    value |= ((std::to_underlying(reg) & 0b111) << 3U);
    value |= (mod << 6U);
    program->insert_byte(value);

    if (mod != 0b11 && (std::to_underlying(rm) & 0b111) == 0b100) {
      // Special case for SP or r12 displacements
      program->insert_byte(0x24); // SIB byte
    }
  }

  constexpr auto insert_modrm(uint8_t digit, unsigned mod, Register rm)
      -> void {
    assert(mod <= 0b11);

    uint8_t value = 0;
    value |= (std::to_underlying(rm) & 0b111);
    value |= (digit << 3U);
    value |= (mod << 6U);
    program->insert_byte(value);

    if (mod != 0b11 && (std::to_underlying(rm) & 0b111) == 0b100) {
      // Special case for SP or r12 displacements
      program->insert_byte(0x24); // SIB byte
    }
  }

  template <typename RegOrDigit>
  constexpr auto insert_base_offset_immediate(RegOrDigit reg, Register base,
                                              uint32_t offset) -> void {
    if (offset == 0 && (std::to_underlying(base) & 0b111) != 0b101) {
      insert_modrm(reg, /*mod=*/0b00, /*rm=*/base);
    } else if (offset <= 127) {
      insert_modrm(reg, /*mod=*/0b01, /*rm=*/base);
      program->insert_immediate((uint8_t)offset);
    } else {
      insert_modrm(reg, /*mod=*/0b10, /*rm=*/base);
      program->insert_immediate(offset);
    }
  }

  constexpr auto insert_reference_to_label(Label label, Fixup::Type type)
      -> void {
    assembler->label_to_fixups[label].push_back(
        {program->current_offset(), type});

    switch (type) {
    case Fixup::Type::relative_4_bytes:
      program->insert_bytes({{0, 0, 0, 0}});
      break;
    }
  }

public:
  explicit constexpr FunctionBuilder(Assembler *assembler, bool requires_return)
      : assembler{assembler}, program{&assembler->program},
        requires_return{requires_return} {}

  ~FunctionBuilder() {
    while (not saved.empty()) {
      insert_pop(saved.back());
      saved.pop_back();
    }

    if (requires_return) {
      program->insert_byte(/*ret*/ 0xc3);
    }
    program->insert_padding(16);
  }

  constexpr auto allocate_label() -> Label {
    return assembler->allocate_label();
  }

  constexpr auto mark_saved(Register reg) -> void {
    saved.push_back(reg);
    insert_push(reg);
  }

  constexpr auto attach_label(Label label) -> void {
    assert(assembler->label_to_location.count(label) == 0);
    assembler->label_to_location[label] = program->current_offset();
  }

  constexpr auto insert_load_imm32(Register reg, uint32_t imm) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0xb8U + (std::to_underlying(reg) & 0b111));
    program->insert_immediate(imm);
  }

  constexpr auto insert_or_imm8(Register reg, uint8_t imm) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x83U);
    insert_modrm(1, /*mod=*/0b11, reg);
    program->insert_immediate(imm);
  }

  constexpr auto insert_xor(Register dst, Register src) -> void {
    maybe_insert_rex(dst, src);
    program->insert_byte(0x31);
    insert_modrm(/*reg=*/src, 0b11, /*rm=*/dst);
  }

  constexpr auto insert_load_imm64(Register reg, uint64_t imm) -> void {
    insert_rex(/*is_operand=*/true, reg);
    program->insert_byte(0xb8U + (std::to_underlying(reg) & 0b111));
    program->insert_immediate(imm);
  }

  constexpr auto insert_mov32(Register dst, Register src) -> void {
    maybe_insert_rex(src, dst);
    program->insert_byte(0x8bU);
    insert_modrm(/*reg=*/dst, /*mod=*/0b11, /*rm=*/src);
  }

  constexpr auto insert_mov64(Register dst, Register src) -> void {
    insert_rex(true, src, dst);
    program->insert_byte(0x8bU);
    insert_modrm(/*reg=*/dst, /*mod=*/0b11, /*rm=*/src);
  }

  constexpr auto insert_load32(Register dst, Register base, uint32_t offset)
      -> void {
    maybe_insert_rex(base, dst);
    program->insert_byte(0x8bU);
    insert_base_offset_immediate(dst, base, offset);
  }

  constexpr auto insert_load64(Register dst, Register base, uint32_t offset)
      -> void {
    insert_rex(true, base, dst);
    program->insert_byte(0x8bU);
    insert_base_offset_immediate(dst, base, offset);
  }

  constexpr auto insert_store_imm32(Register dst_base, uint32_t dst_offset,
                                    uint32_t value) -> void {
    maybe_insert_rex(dst_base);

    program->insert_byte(0xc7U);
    insert_base_offset_immediate(0, dst_base, dst_offset);
    program->insert_immediate(value);
  }

  constexpr auto insert_store32(Register dst_base, uint32_t dst_offset,
                                Register src) -> void {
    maybe_insert_rex(dst_base, src);
    program->insert_byte(0x89U);
    insert_base_offset_immediate(src, dst_base, dst_offset);
  }

  constexpr auto insert_store64(Register dst_base, uint32_t dst_offset,
                                Register src) -> void {
    insert_rex(true, dst_base, src);
    program->insert_byte(0x89U);
    insert_base_offset_immediate(src, dst_base, dst_offset);
  }

  template <typename Ret, typename... Args>
  constexpr auto insert_call(Ret (*fn)(Args...)) -> void {
    static constexpr auto call_address = Register::rax;

    // Issue an indirect call instruction
    // TODO: improve to an actual immediate
    insert_load_imm64(call_address, (uintptr_t)fn);
    program->insert_byte(0xff);
    insert_modrm(2, /*mod=*/0b11, /*rm=*/call_address);
  }

  constexpr auto insert_cmp_r8_imm8(Register reg, uint8_t compare_to) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x80);
    insert_modrm(7, /*mod=*/0b11, reg);
    program->insert_immediate(compare_to);
  }

  constexpr auto insert_cmp_r32_imm32(Register reg, uint32_t compare_to)
      -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x81);
    insert_modrm(7, /*mod=*/0b11, reg);
    program->insert_immediate(compare_to);
  }

  constexpr auto insert_set_if_zero(Register dst) -> void {
    insert_rex(/*is_operand=*/false, dst);
    program->insert_byte(0x0F);
    program->insert_byte(0x94);
    insert_modrm(0, /*mod=*/0b11, dst);
  }

  constexpr auto insert_shiftl_r64(Register reg, uint8_t imm) -> void {
    insert_rex(true, reg);
    program->insert_byte(0xc1);
    insert_modrm(4, /*mod=*/0b11, reg);
    program->insert_immediate(imm);
  }

  constexpr auto insert_shiftr_r64(Register reg, uint8_t imm) -> void {
    insert_rex(true, reg);
    program->insert_byte(0xc1);
    insert_modrm(5, /*mod=*/0b11, reg);
    program->insert_immediate(imm);
  }

  constexpr auto insert_or_r64(Register dst, Register src) -> void {
    insert_rex(true, dst, src);
    program->insert_byte(0x09);
    insert_modrm(src, /*mod=*/0b11, dst);
  }

  constexpr auto insert_test_imm8(Register reg, uint8_t compare_to) -> void {
    maybe_insert_rex(reg, {}, /*is_u8=*/true);
    program->insert_byte(0xf6);
    insert_modrm(0, /*mod=*/0b11, reg);
    program->insert_immediate(compare_to);
  }

  constexpr auto insert_test(Register r1, Register r2) -> void {
    maybe_insert_rex(r1, r2, /*is_u8=*/true);
    program->insert_byte(0x84);
    insert_modrm(r2, /*mod=*/0b11, r1);
  }

  constexpr auto insert_load32_cmp_imm(Register base, uint32_t offset,
                                       uint32_t compare_to) -> void {
    maybe_insert_rex(base);

    if (compare_to <= 127) {
      // Special case for short ASCII compares
      program->insert_byte(0x83);
      insert_base_offset_immediate(7, base, offset);
      program->insert_immediate((uint8_t)compare_to);
    } else {
      // Full imm32 compare
      program->insert_byte(0x81);
      insert_base_offset_immediate(7, base, offset);
      program->insert_immediate(compare_to);
    }
  }

  constexpr auto insert_load_cmp32(Register base, uint32_t offset,
                                   Register compare_to) -> void {
    maybe_insert_rex(base, compare_to);
    program->insert_byte(0x39);
    insert_base_offset_immediate(compare_to, base, offset);
  }

  constexpr auto insert_jump_if_zero_flag(Label target_if_zero) -> void {
    program->insert_bytes({{0x0f, 0x84}});
    insert_reference_to_label(target_if_zero, Fixup::Type::relative_4_bytes);
  }

  constexpr auto insert_jump_if_not_zero_flag(Label target_if_zero) -> void {
    program->insert_bytes({{0x0f, 0x85}});
    insert_reference_to_label(target_if_zero, Fixup::Type::relative_4_bytes);
  }

  constexpr auto insert_jump_if_carry_flag(Label target_if_zero) -> void {
    program->insert_bytes({{0x0f, 0x82}});
    insert_reference_to_label(target_if_zero, Fixup::Type::relative_4_bytes);
  }

  constexpr auto insert_jump_if_not_carry_nor_zero_flag(Label target_if_zero)
      -> void {
    program->insert_bytes({{0x0f, 0x87}});
    insert_reference_to_label(target_if_zero, Fixup::Type::relative_4_bytes);
  }

  constexpr auto insert_jump_if_bool_false(Register reg, Label target_if_false)
      -> void {
    insert_test(reg, reg);
    insert_jump_if_zero_flag(target_if_false);
  }

  constexpr auto insert_jump_if_bool_true(Register reg, Label target_if_true)
      -> void {
    insert_test(reg, reg);
    insert_jump_if_not_zero_flag(target_if_true);
  }

  constexpr auto insert_jump(Label target) -> void {
    program->insert_byte(0xe9);
    insert_reference_to_label(target, Fixup::Type::relative_4_bytes);
  }

  constexpr auto insert_push(Register reg) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x50 + (std::to_underlying(reg) & 0b111));
  }

  constexpr auto insert_pop(Register reg) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x58 + (std::to_underlying(reg) & 0b111));
  }

  constexpr auto insert_add(Register reg, uint8_t imm) -> void {
    maybe_insert_rex(reg);
    program->insert_byte(0x83);
    insert_modrm(0, /*mod=*/0b11, reg);
    program->insert_immediate(imm);
  }
};

template <typename Builder>
constexpr auto Assembler::build_function(Label fn_label, Builder &&fn_builder)
    -> void {
  program.insert_padding(16);
  assert(label_to_location.count(fn_label) == 0);
  label_to_location[fn_label] = program.current_offset();

  FunctionBuilder builder{this, true};
  fn_builder(builder);
}

template <typename Builder>
constexpr auto Assembler::build_out_of_line_block(Label fn_label,
                                                  Builder &&fn_builder)
    -> void {
  program.insert_padding(16);
  assert(label_to_location.count(fn_label) == 0);
  label_to_location[fn_label] = program.current_offset();

  FunctionBuilder builder{this, false};
  fn_builder(builder);
}

} // namespace uregex::jit
