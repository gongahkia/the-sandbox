#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <functional>
#include <algorithm>

class PikLang {
public:
    void run() {
        std::string line;
        while (true) {
            std::cout << "PikLang> ";
            std::getline(std::cin, line);
            if (line == "exit") break;

            try {
                if (line.substr(0, 6) == "print ") {
                    auto value = eval(line.substr(6));
                    std::cout << value << std::endl;
                } else {
                    eval(line);
                }
            } catch (const std::exception& e) {
                std::cerr << "Error: " << e.what() << std::endl;
            }
        }
    }

private:
    std::unordered_map<std::string, int> variables;
    std::unordered_map<std::string, std::function<int(std::vector<int>)>> functions;

    int eval(const std::string& expression) {
        if (expression.find('=') != std::string::npos) {
            auto pos = expression.find('=');
            auto var = expression.substr(0, pos);
            auto value = eval(trim(expression.substr(pos + 1)));
            variables[var] = value;
            return value;
        }
        if (isNumber(expression)) {
            return std::stoi(expression);
        }
        if (variables.find(expression) != variables.end()) {
            return variables[expression];
        }
        if (expression.substr(0, 2) == "if") {
            return evalIf(expression);
        }
        if (expression.substr(0, 4) == "fun ") {
            defineFunction(expression);
            return 0;
        }
        if (expression.find('(') != std::string::npos) {
            return callFunction(expression);
        }
        return evalArithmetic(expression);
    }

    int evalIf(const std::string& expression) {
        auto conditionStart = expression.find('(') + 1;
        auto conditionEnd = expression.find(')');
        auto condition = expression.substr(conditionStart, conditionEnd - conditionStart);

        auto body = trim(expression.substr(conditionEnd + 1));
        
        if (eval(condition)) {
            return eval(body);
        }
        return 0;
    }

    void defineFunction(const std::string& expression) {
        auto funcNameStart = expression.find("fun ") + 4;
        auto funcNameEnd = expression.find('(');
        auto funcName = trim(expression.substr(funcNameStart, funcNameEnd - funcNameStart));

        auto paramsStart = funcNameEnd + 1;
        auto paramsEnd = expression.find(')');
        auto params = split(trim(expression.substr(paramsStart, paramsEnd - paramsStart)), ',');

        auto bodyStart = paramsEnd + 1;
        auto body = trim(expression.substr(bodyStart));

        functions[funcName] = [this, params, body](std::vector<int> args) -> int {
            for (size_t i = 0; i < params.size(); ++i) {
                variables[trim(params[i])] = args[i];
            }
            return eval(body);
        };
    }

    int callFunction(const std::string& expression) {
        auto funcNameEnd = expression.find('(');
        auto funcName = trim(expression.substr(0, funcNameEnd));
        
        auto argsStart = funcNameEnd + 1;
        auto argsEnd = expression.find(')');
        
        auto argsStr = trim(expression.substr(argsStart, argsEnd - argsStart));
        
        std::vector<int> args;
        
        for (const auto& arg : split(argsStr, ',')) {
            args.push_back(eval(trim(arg)));
        }

        if (functions.find(funcName) != functions.end()) {
            return functions[funcName](args);
        }
        
        throw std::runtime_error("Function not defined: " + funcName);
    }

    int evalArithmetic(const std::string& expression) {
        if (expression.find('+') != std::string::npos) {
            auto parts = split(expression, '+');
            return eval(trim(parts[0])) + eval(trim(parts[1]));
        } else if (expression.find('-') != std::string::npos) {
            auto parts = split(expression, '-');
            return eval(trim(parts[0])) - eval(trim(parts[1]));
        } else if (expression.find('*') != std::string::npos) {
            auto parts = split(expression, '*');
            return eval(trim(parts[0])) * eval(trim(parts[1]));
        } else if (expression.find('/') != std::string::npos) {
            auto parts = split(expression, '/');
            return eval(trim(parts[0])) / eval(trim(parts[1]));
        }
        
        throw std::runtime_error("Invalid arithmetic expression: " + expression);
    }

    bool isNumber(const std::string& str) const {
       return !str.empty() && std::all_of(str.begin(), str.end(), ::isdigit);
    }

    std::vector<std::string> split(const std::string& s, char delimiter) const {
       std::vector<std::string> tokens;
       std::stringstream ss(s);
       std::string item;
       while (std::getline(ss, item, delimiter)) {
           tokens.push_back(item);
       }
       return tokens;
    }

    inline std::string trim(const std::string& str) const {
       size_t first = str.find_first_not_of(' ');
       size_t last = str.find_last_not_of(' ');
       return str.substr(first, last - first + 1);
    }
};

int main() {
    PikLang lang;
    lang.run();
    return 0;
}