#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <tuple>
#include "nlohmann/json.hpp"
#include "curl/curl.h"
#include <chrono>

using namespace std;
using json = nlohmann::json;

class RelictMatQueryModel {
public:
    RelictMatQueryModel() : result("1"), error_code("! :-( !") {}

    bool membershipQuery(const string& word) {
        string query_word = word.empty() ? "ε" : word;
        json query_body;
        query_body["word"] = query_word;
        //cout << query_body << std::endl;

        try {
            string response = postRequest("http://127.0.0.1:8095/checkWord", query_body);
            auto jsonResponse = json::parse(response);
            return jsonResponse["response"].get<bool>(); // Извлекаем булево значение
        }
        catch (const std::exception& e) {
            cerr << "Произошла ошибка при проверке на включение :-( => " << e.what() << endl;
            return false; // Возвращаем значение по умолчанию
        }
    }

    tuple<int, string> equivalenceQuery(const string& S, const string& S_extra, const string& E, const string& table) {
        json query_body;
        query_body["main_prefixes"] = S;
        query_body["non_main_prefixes"] = S_extra;
        query_body["suffixes"] = E;
        query_body["table"] = table;
        //cout << "QUERY BODY:" << std::endl;
        //cout << query_body << std::endl;

        try {
            string response = postRequest("http://127.0.0.1:8095/checkTable", query_body);
            auto jsonResponse = json::parse(response);

            // Проверяем, что поле "type" существует и не равно null
            if (!jsonResponse["type"].is_null()) {
                bool type_value = jsonResponse["type"].get<bool>();

                if (type_value && !jsonResponse["response"].is_null()) {
                    return { 0, jsonResponse["response"].get<string>() };
                }
                else if (!type_value) {
                    return { 1, jsonResponse["response"].get<string>() };
                }
            }

            // Если поле "type" равно null или другой случай
            return { 2, "" };
        }
        catch (const std::exception& e) {
            std::cerr << "Произошла ошибка при проверке на эквивалентность :-( => " << e.what() << std::endl;
            return { 2, "" };
        }
    }

private:
    std::string result;
    std::string error_code;

    static std::string postRequest(const std::string& url, const json& data) {
        CURL* curl;
        CURLcode res;
        std::string response;
        struct curl_slist* headers = nullptr;

        curl = curl_easy_init();
        if (curl) {
            std::string jsonData = data.dump();
            headers = curl_slist_append(headers, "Content-Type: application/json");
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_POSTFIELDS, jsonData.c_str());
            curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeCallback);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

            res = curl_easy_perform(curl);
            curl_easy_cleanup(curl);
            curl_slist_free_all(headers);

            if (res != CURLE_OK) {
                throw std::runtime_error(curl_easy_strerror(res));
            }
        }
        return response;
    }

    static size_t writeCallback(void* contents, size_t size, size_t nmemb, std::string* userData) {
        userData->append(static_cast<char*>(contents), size * nmemb);
        return size * nmemb;
    }
};

class LStarAlgorithm {
public:
    LStarAlgorithm(const std::vector<std::string>& A, RelictMatQueryModel& mat_model)
        : mat(mat_model), A(A) {
        S.push_back("");
        E.push_back("");
        initializeTable();
    }

    void run() {
        while (true) {
            bool closed;
            std::string s1, a;
            std::tie(closed, s1, a) = isClosed();
            if (!closed) {
                S.push_back(s1 + a);
                extendTable();
                continue;
            }

            bool consistent;
            std::string s1_c, s2_c, a_c, e_c;
            std::tie(consistent, s1_c, s2_c, a_c, e_c) = isConsistent();
            if (!consistent) {
                E.push_back(a_c + e_c);
                extendTable();
                continue;
            }

            auto hypothesis = buildHypothesis();
            int equivalent;
            std::string counterexample;
            std::tie(equivalent, counterexample) = mat.equivalenceQuery(std::get<0>(hypothesis), std::get<1>(hypothesis), std::get<2>(hypothesis), std::get<3>(hypothesis));
            if (equivalent == 2) {
                std::cout << "Алгоритм завершен. Модель правильна." << std::endl;
                printHypothesis(std::get<0>(hypothesis), std::get<1>(hypothesis), std::get<2>(hypothesis), std::get<3>(hypothesis));
                break;
            }
            else if (equivalent == 1) {
                for (size_t i = 1; i <= counterexample.size(); ++i) {
                    std::string prefix = counterexample.substr(0, i);
                    if (std::find(S.begin(), S.end(), prefix) == S.end()) {
                        S.push_back(prefix);
                    }
                }
                extendTable();
            }
            else if (equivalent == 0) {
                for (size_t i = 0; i < counterexample.size(); ++i) {
                    std::string suffix = counterexample.substr(i);
                    if (std::find(E.begin(), E.end(), suffix) == E.end()) {
                        E.push_back(suffix);
                    }
                }
                extendTable();
            }
        }
    }

private:
    RelictMatQueryModel& mat;
    vector<string> A;
    vector<string> S;
    vector<string> E;
    map<pair<string, string>, bool> T;

    void initializeTable() {
        for (const auto& s : S) {
            for (const auto& e : E) {
                T[{s, e}] = mat.membershipQuery(s + e);
            }
        }
    }

    tuple<bool, string, string> isClosed() {
        for (const auto& s1 : S) {
            for (const auto& a : A) {
                vector<bool> row_s1a = getRow(s1 + a);
                if (std::none_of(S.begin(), S.end(), [&](const string& s) { return getRow(s) == row_s1a; })) {
                    return { false, s1, a };
                }
            }
        }
        return { true, "", "" };
    }

    tuple<bool, string, string, string, string> isConsistent() {
        for (size_t i = 0; i < S.size(); ++i) {
            for (size_t j = i + 1; j < S.size(); ++j) {
                const string s1 = S[i];
                const string s2 = S[j];
                if (getRow(s1) == getRow(s2)) {
                    for (const auto& a : A) {
                        for (const auto& e : E) {
                            if (T[{s1 + a, e}] != T[{s2 + a, e}]) {
                                return { false, s1, s2, a, e };
                            }
                        }
                    }
                }
            }
        }
        return { true, "", "", "", "" };
    }

    vector<bool> getRow(const string& s) {
        vector<bool> row;
        for (const auto& e : E) {
            row.push_back(T[{s, e}]);
        }
        return row;
    }

    void extendTable() {
        vector<string> tempA = { "" };
        tempA.insert(tempA.end(), A.begin(), A.end());
        for (const auto& s : S) {
            for (const auto& a : tempA) {
                for (const auto& e : E) {
                    if (T.find({ s + a, e }) == T.end()) {
                        T[{s + a, e}] = mat.membershipQuery(s + a + e);
                    }
                }
            }
        }
    }

    tuple<string, string, string, string> buildHypothesis() {
        vector<string> non_main_prefixes;
        for (const auto& entry : T) {
            const auto& key = entry.first;
            if ((find(S.begin(), S.end(), key.first) == S.end()) &&
                (find(non_main_prefixes.begin(), non_main_prefixes.end(), key.first) == non_main_prefixes.end())) {
                non_main_prefixes.push_back(key.first);
            }
        }

        // Создание строки таблицы
        string table_string = "";
        for (const auto& s : S) {
            for (const auto& e : E) {
                table_string += (T[{s, e}] ? "1 " : "0 ");
            }
        }

        for (const auto& s : non_main_prefixes) {
            for (const auto& e : E) {
                table_string += (T[{s, e}] ? "1 " : "0 ");
            }
        }

        // Создание строк Sm, Em, Nm
        string Sm = "";
        for (const auto& x : S) {
            Sm += (x.empty() ? "ε " : x + " ");
        }

        string Em = "";
        for (const auto& x : E) {
            Em += (x.empty() ? "ε " : x + " ");
        }

        string Nm = "";
        for (const auto& x : non_main_prefixes) {
            Nm += (x.empty() ? "ε " : x + " ");
        }

        // Убираем последние пробелы
        if (!Sm.empty()) Sm.pop_back();
        if (!Em.empty()) Em.pop_back();
        if (!Nm.empty()) Nm.pop_back();
        if (!table_string.empty()) table_string.pop_back();

        return make_tuple(Sm, Nm, Em, table_string);
    }

    void printHypothesis(const string& S, const string& S_extra, const string& E, const string& table) {

        // Печать содержимого кортежа
        cout << "S: " << S << endl;
        cout << "S_extra: " << S_extra << endl;
        cout << "E: " << E << endl;
        cout << "table: " << table << endl;
    }
};

int main() {
    auto start = std::chrono::high_resolution_clock::now();
    std::vector<std::string> A = { "L", "R" };
    RelictMatQueryModel mat_model;
    LStarAlgorithm l_star(A, mat_model);
    l_star.run();
    auto end = std::chrono::high_resolution_clock::now();

    std::chrono::duration<double> duration = end - start;
    std::cout << "Время выполнения программы: " << duration.count() << " секунд" << std::endl;

    return 0;
}
