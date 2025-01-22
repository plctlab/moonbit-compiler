#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <cstring>
#include <functional>
#include <map>
#include <codecvt>
#include <locale>
#include <cstdint>

const int unit = unit;

// Obtain a list of instructions from the leading label of the basic block.
std::map<std::string, std::vector<std::string>> blocks;

// Obtain the list of arguments from a function name.
std::map<std::string, std::vector<std::string>> fns;

// Values of registers used when interpreting.
// TODO: currently no FP supported.
std::map<std::string, int64_t> regs;
std::map<std::string, int64_t> labels;

std::vector<std::string> split(std::string s, std::string delim) {
    size_t start = 0, end;
    size_t delim_len = delim.length();
    std::vector<std::string> res;

    if (s.length() == 0)
        return res;

    while ((end = s.find(delim, start)) != std::string::npos) {
        res.push_back(s.substr(start, end - start));
        start = end + delim_len;
    }

    res.push_back(s.substr(start));
    return res;
}

// Remove leading and trailing spaces of `s`.
std::string strip(const std::string& s) {
    size_t start = s.find_first_not_of(" \t\n\r");
    if (start == std::string::npos) {
        return "";
    }

    size_t end = s.find_last_not_of(" \t\n\r");

    // Note: second argument is the length of substring
    return s.substr(start, end - start + 1);
}

int int_of(std::string s) {
    return atoi(s.c_str());
}

#define RTYPE(name, op) std::make_pair(name, [](int64_t x, int64_t y) { return x op y; })
#define MEM(name, type) std::make_pair(name, [](int64_t x, int offset) { return *((type*) (x + offset)); })
#define ITYPE(name, op) std::make_pair(name, [](int64_t x, int imm) -> int64_t { return x op imm; })
#define RTYPEW(name, op) std::make_pair(name, [](int64_t x, int64_t y) { return (int)x op (int)y; })
#define RTYPEU(name, op) std::make_pair(name, [](int64_t x, int64_t y) { return (uint64_t)x op (uint64_t)y; })
#define RTYPEUW(name, op) std::make_pair(name, [](int64_t x, int64_t y) { return (unsigned)x op (unsigned)y; })
#define ITYPEW(name, op) std::make_pair(name, [](int64_t x, int imm) -> int64_t { return (int)x op imm; })
#define VAL(i) regs[args[i]]

#ifdef VERBOSE
#define OUTPUT(name, value) std::cerr << "\t" << name << " = " << value << "\n\n"
#define SAY(str) std::cerr << str << "\n"
#else
#define OUTPUT(name, value)
#define SAY(str)
#endif

// Argument `label` is where we start interpreting.
int64_t interpret(std::string label) {
    static std::map<std::string, std::function<int64_t (int64_t, int64_t)>> rtype = {
        RTYPE("add", +),
        RTYPE("sub", -),
        RTYPE("mul", *),
        RTYPE("div", /),
        RTYPE("mod", %),
        RTYPE("le", <),
        RTYPE("leq", <=),
        RTYPE("ge", >),
        RTYPE("geq", >=),
        RTYPE("eq", ==),
        RTYPE("ne", !=),
        RTYPE("and", &),
        RTYPE("or", |),
        RTYPE("xor", ^),
        RTYPE("sll", <<),
        RTYPE("sra", >>),
        RTYPEW("addw", +),
        RTYPEW("subw", -),
        RTYPEW("mulw", *),
        RTYPEW("divw", /),
        RTYPEW("modw", %),
        RTYPEW("sllw", <<),
        RTYPEW("srlw", >>),
        RTYPEU("addu", +),
        RTYPEU("subu", -),
        RTYPEU("mulu", *),
        RTYPEU("divu", /),
        RTYPEU("modu", %),
        RTYPEU("leu", <),
        RTYPEU("lequ", <=),
        RTYPEU("geu", >),
        RTYPEU("gequ", >=),
        RTYPEU("equ", ==),
        RTYPEU("neu", !=),
        RTYPEUW("adduw", +),
        RTYPEUW("subuw", -),
        RTYPEUW("muluw", *),
        RTYPEUW("divuw", /),
        RTYPEUW("moduw", %),
    };

    static std::map<std::string, std::function<int64_t (int64_t, int)>> load = {
        MEM("lb", char),
        MEM("lh", char16_t),
        MEM("lw", int),
        MEM("ld", int64_t),
    };

    static std::map<std::string, std::function<int64_t (int64_t, int)>> itype = {
        ITYPE("addi", +),
        ITYPE("srai", >>),
        ITYPE("slli", <<),
        ITYPE("andi", &),
        ITYPE("ori", |),
        ITYPE("xori", ^),
        ITYPE("slti", <),
        ITYPEW("addiw", +),
        ITYPEW("sraiw", >>),
        ITYPEW("slliw", <<),
        ITYPEW("sltiw", <),
    };

    std::string prev;

    for (;;) {
        if (!blocks.contains(label)) {
            std::cerr << "Unknown label: " << label << std::endl;
            exit(3);
        }
        
        for (auto x : blocks[label]) {
            SAY(x);
            auto args = split(x, " ");
            auto op = args[0];

            if (rtype.contains(op)) {
                VAL(1) = rtype[op](VAL(2), VAL(3));
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (load.contains(op)) {
                VAL(1) = load[op](VAL(2), int_of(args[3]));
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (itype.contains(op)) {
                VAL(1) = itype[op](VAL(2), int_of(args[3]));
                OUTPUT(args[1], VAL(1));
                continue;
            }

            // Logical left-shift family
            if (op == "srli") {
                VAL(1) = ((uint64_t) VAL(2)) >> int_of(args[3]);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "srliw") {
                VAL(1) = ((unsigned) VAL(2)) >> int_of(args[3]);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "srl") {
                VAL(1) = ((uint64_t) VAL(2)) >> VAL(3);
                OUTPUT(args[1], VAL(1));
                continue;
            }
            
            if (op == "srlw") {
                VAL(1) = ((unsigned) VAL(2)) >> VAL(3);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            // Less than family, for unsigned values
            if (op == "sltiu") {
                VAL(1) = ((uint64_t) VAL(2)) < int_of(args[3]);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "sltiuw") {
                VAL(1) = ((unsigned) VAL(2)) < int_of(args[3]);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            // Other operations

            if (op == "neg") {
                VAL(1) = -VAL(2);
                continue;
            }

            if (op == "not") {
                VAL(1) = ~VAL(2);
                continue;
            }

            if (op == "sd" || op == "sb" || op == "sw" || op == "sh") {
                auto rd = VAL(1);
                auto rs = VAL(2);
                auto offset = int_of(args[3]);

                if (op == "sb")
                    *((char*)(rs + offset)) = rd;
                if (op == "sh")
                    *((char16_t*)(rs + offset)) = rd;
                if (op == "sw")
                    *((int*)(rs + offset)) = rd;
                if (op == "sd")
                    *((int64_t*)(rs + offset)) = rd;
            
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "call") {
                auto before = regs;

                auto fn = args[2];
                SAY("call " << fn);
                for (int i = 0; i < fns[fn].size(); i++) {
                    regs[fns[fn][i]] = VAL(i + 3);
                    OUTPUT(fns[fn][i], VAL(i + 3));
                }

                auto value = interpret(fn);
                regs = before;
                VAL(1) = value;

                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "call_indirect") {
                auto before = regs;
                
                // The pointer is a string of function name
                std::string fn((char*) VAL(2));
                SAY("call indirect " << fn);
                for (int i = 0; i < fns[fn].size(); i++) {
                    regs[fns[fn][i]] = VAL(i + 3);
                    OUTPUT(fns[fn][i], VAL(i + 3));
                }
                
                auto value = interpret(fn);
                regs = before;
                VAL(1) = value;

                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "call_libc") {
                for (int i = 3; i < args.size(); i++)
                    OUTPUT(args[i], VAL(i));
                
                if (args[2] == "puts") {
                    // Make the output string null-terminated
                    int len = *(int*) (VAL(3) - 4);
                    auto ptr = new char16_t[len + 1];
                    memcpy(ptr, (void*) VAL(3), len * 2);
                    ptr[len] = 0;

                    std::u16string utf16_str(ptr);

                    // Convert to UTF-8, so that cout can output it
                    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
                    std::string utf8_string = convert.to_bytes(utf16_str);

                    for (int i = 0; i < len * 2; i++)
                        OUTPUT("char of byte " << i, *(char*) (ptr + i));
                    std::cout << utf8_string << std::endl;
                    continue;
                }

                if (args[2] == "malloc") {
                    VAL(1) = (int64_t) new char[VAL(3)];
                    continue;
                }

                if (args[2] == "strlen") {
                    VAL(1) = (int64_t) strlen((char*) VAL(3));
                    continue;
                }

                if (args[2] == "memset") {
                    memset((void*) VAL(3), VAL(4), VAL(5));
                    VAL(1) = unit;
                    continue;
                }

                if (args[2] == "memcpy") {
                    memcpy((void*) VAL(3), (void*) VAL(4), VAL(5));
                    continue;
                }

                if (args[2] == "abort")
                    abort();

                std::cerr << "Unknown libc call: " << args[2] << std::endl;
                exit(3);
            }

            if (op == "malloc") {
                auto len = int_of(args[2]);

                VAL(1) = (int64_t) new char[len];
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "alloca") {
                auto len = int_of(args[2]);

                VAL(1) = (int64_t) alloca(len);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "phi") {
                bool is_bad = true;

                for (int i = 2; i < args.size(); i += 2) {
                    auto var = args[i];
                    auto plabel = args[i + 1];
                    if (plabel == prev) {
                        VAL(1) = regs[var];
                        is_bad = false;
                        OUTPUT(args[1], VAL(1));
                        break;
                    }
                }
                
                if (is_bad) {
                    std::cerr << "Bad phi\n";
                    exit(3);
                }
                continue;
            }

            if (op == "li") {
                // This can be larger than the size of int
                std::stringstream ss(args[2]);
                int64_t rs;
                ss >> rs;

                VAL(1) = rs;
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "mv") {
                VAL(1) = VAL(2);
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "la") {
                VAL(1) = labels[args[2]];
                OUTPUT(args[1], VAL(1));
                continue;
            }

            if (op == "j") {
                prev = label;
                label = args[1];
                break;
            }
            
            if (op == "jr") {
                prev = label;
                label = std::string((char*) VAL(1));
                SAY("jump to " << label);
                break;
            }

            if (op == "br") {
                prev = label;
                label = VAL(1) ? args[2] : args[3];
                break;
            }

            if (op == "return") {
                return VAL(1);
            }

            std::cerr << "Bad instruction: " << x << std::endl;
            exit(4);
        }
    }
}

// Note: Since SSA form is subject to change and is only internally used,
// we don't check if the input is legal.
// Please feed this program only the SSA generated by the same version of `moonc`.
int main(int argc, char** argv) {
    if (argc != 2) {
        std::cerr << "Usage: verifier <file name>\n";
        return 1;
    }

    std::ifstream ifs(argv[1]);

    // Label and instructions of a basic block
    std::string label;
    std::vector<std::string> inst;

    while (ifs) {
        std::string str;
        std::getline(ifs, str);

        str = strip(str);

        // This is a function. We only need to know its arguments.
        if (str.starts_with("fn ")) {
            // Remove the leading "fn "
            std::stringstream ss(str.substr(3));
            std::string fn_name;
            ss >> fn_name;

            // A comma-separated list.
            std::string rest;
            std::getline(ss, rest);

            // Remove leading " (" and trailing ") {"
            rest = rest.substr(2, rest.size() - 5);

            // Store argument list
            fns[fn_name] = split(rest, ", ");

            // Function is also a label
            // We need to get a string for it
            char* str = new char[fn_name.size() + 1];
            strcpy(str, fn_name.c_str());
            labels[fn_name] = (int64_t) str;
            continue;
        }

        // This is a global array / global variable
        // We need to allocate space for them 
        if (str.starts_with("global ")) {

            if (str.starts_with("global array ")) {
                // Remove leading "global array " and trailing ":"
                std::stringstream ss(str.substr(13, str.size() - 14));
                int elem_size;
                std::string name;

                ss >> elem_size >> name;

                // Look at the next line, i.e. contents of the array
                std::string content;
                std::getline(ifs, content);
                content = strip(content);

                auto elems = split(content, ", ");
                size_t len = elems.size();
                char* space = nullptr;

                // vtables are special: no length needed, and the elements are all function pointers
                // here we'll store function pointers as `char*` - the function name
                // jumptables are also the same
                if (name.starts_with("vtable_") || name.starts_with("jumptable_")) {
                    space = new char[len * 8];
                    for (int i = 0; i < len; i++) {
                        char* name = new char[elems[i].size() + 1];
                        strcpy(name, elems[i].c_str());
                        *(char**)(space + i * 8) = name;
                    }
                }

                // For other arrays, the first element is an integer, and others are of `elem_size`
                else {
                    space = new char[(len - 1) * elem_size + 4];
                    * (int*) space = int_of(elems[0]);
                    for (int i = 1; i < len; i++)
                        space[(i - 1) * elem_size + 4] = int_of(elems[i]);
                }
                labels[name] = (int64_t) space;
                continue;
            }

            // Remove "global "
            std::stringstream ss(str.substr(7));

            // This is simply a global variable
            std::string name; int size;
            ss >> name >> size;
            labels[name] = (int64_t) new char[size];
        }
        
        // A new label is met. Refresh `label` and `inst`.
        if (str.ends_with(":")) {
            label = str.substr(0, str.size() - 1);
            inst.clear();
            continue;
        }

        inst.push_back(str);

        // Finish of a block. Record it in `blocks`.
        if (str.starts_with("j ") || str.starts_with("br ") || str.starts_with("return ") || str.starts_with("jr "))
            blocks[label] = inst;
    }

    regs["_"] = unit;
    interpret("_start");
}
