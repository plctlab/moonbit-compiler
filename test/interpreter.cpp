#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <string>
#include <cstring>
#include <functional>
#include <map>

const int unit = unit;

// Obtain a list of instructions from the leading label of the basic block.
std::map<std::string, std::vector<std::string>> blocks;

// Obtain the list of arguments from a function name.
std::map<std::string, std::vector<std::string>> fns;

// Values of registers used when interpreting.
// TODO: currently no FP supported.
std::map<std::string, uint64_t> regs;

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

#define RTYPE(name, op) std::make_pair(name, [](uint64_t x, uint64_t y) { return x op y; })
#define MEM(name, type) std::make_pair(name, [](uint64_t x, int offset) { return *((type*) x + offset); })
#define VAL(i) regs[args[i]]
// Argument `label` is where we start interpreting.
uint64_t interpret(std::string label) {
    static std::map<std::string, std::function<uint64_t (uint64_t, uint64_t)>> rtype = {
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
        RTYPE("shl", <<),
        RTYPE("shr", >>),
    };

    static std::map<std::string, std::function<uint64_t (uint64_t, int)>> load = {
        MEM("lb", char),
        MEM("lw", int),
        MEM("ld", uint64_t),
    };

    std::string prev;

    for (;;) {
        for (auto x : blocks[label]) {
            std::cerr << x << std::endl;
            auto args = split(x, " ");
            auto op = args[0];

            if (rtype.contains(op)) {
                VAL(1) = rtype[op](VAL(2), VAL(3));
                continue;
            }

            if (load.contains(op)) {
                VAL(1) = load[op](VAL(2), int_of(args[3]));
                continue;
            }

            if (op == "sd" || op == "sb" || op == "sw") {
                auto rd = VAL(1);
                auto rs = VAL(2);
                auto offset = int_of(args[3]);

                if (op == "sb")
                    *((char*)rs + offset) = rd;
                if (op == "sw")
                    *((int*)rs + offset) = rd;
                if (op == "sd")
                    *((uint64_t*)rs + offset) = rd;
                continue;
            }

            if (op == "call") {
                auto fn = args[2];
                for (int i = 0; i < fns[fn].size(); i++) {
                    regs[fns[fn][i]] = VAL(i + 3);
                }
                VAL(1) = interpret(fn);
                continue;
            }

            if (op == "call_libc") {
                if (args[2] == "puts")
                    puts((char*) VAL(3));

                if (args[2] == "malloc")
                    VAL(1) = (uint64_t) new char(VAL(3));

                if (args[2] == "strlen")
                    VAL(1) = (uint64_t) strlen((char*) VAL(3));

                if (args[2] == "memset")
                    memset((void*) VAL(3), VAL(4), VAL(5)),
                    VAL(1) = unit;

                if (args[2] == "abort")
                    abort();

                continue;
            }

            if (op == "malloc") {
                auto offset = int_of(args[2]);

                VAL(1) = (uint64_t) malloc(offset);
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
                auto rs1 = int_of(args[2]);

                VAL(1) = rs1;
                continue;
            }

            if (op == "mv" || op == "la") {
                VAL(1) = VAL(2);
                continue;
            }

            if (op == "j") {
                prev = label;
                label = args[1];
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
            continue;
        }

        // This is a global array / global variable
        // We need to allocate space for them 
        if (str.starts_with("global ")) {

            if (str.starts_with("global array ")) {
                // Remove leading "global array " and trailing ":"
                auto name = str.substr(13, str.size() - 14);

                // Look at the next line, i.e. contents
                std::string content;
                std::getline(ifs, content);
                strip(content);

                auto elems = split(content, ", ");
                size_t len = elems.size();
                char* space = nullptr;

                // For bytes, the first element is an integer, and others are bytes
                if (name.starts_with("bytes_")) {
                    space = new char[len + 3];
                    * (int*) space = int_of(elems[0]);
                    for (int i = 1; i < len; i++)
                        space[i + 4] = int_of(elems[i]);
                }

                // For vtable, the elements are all function pointers
                // here we'll store function pointers as `char*` - the function name
                if (name.starts_with("vtable_")) {
                    space = new char[len * 8];
                    for (int i = 0; i < len; i++) {
                        char* name = new char[elems[i].size() + 1];
                        strcpy(name, elems[i].c_str());
                        *(uint64_t*)(space + i * 8) = (uint64_t) name;
                    }
                }

                // For strings, they are just c-style strings
                if (name.starts_with("str_")) {
                    space = new char[len + 1];
                    for (int i = 0; i < len; i++) {
                        space[i] = int_of(elems[i]);
                    }
                }

                if (!space) {
                    std::cerr << "Bad SSA: unrecognized global array type\n";
                    return 2;
                }
                regs[name] = (uint64_t) space;
                continue;
            }

            // This is simply a global variable
            std::cerr << "Bad SSA: no global vars yet\n";
            return 2;
        }
        
        // A new label is met. Refresh `label` and `inst`.
        if (str.ends_with(":")) {
            label = str.substr(0, str.size() - 1);
            inst.clear();
            continue;
        }

        inst.push_back(str);

        // Finish of a block. Record it in `blocks`.
        if (str.starts_with("j ") || str.starts_with("br ") || str.starts_with("return "))
            blocks[label] = inst;
    }

    regs["_"] = unit;
    interpret("_start");
}