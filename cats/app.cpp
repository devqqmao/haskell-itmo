#include <iostream>
#include <functional>
#include <stdexcept>
#include <variant>

/*
Общие требования:
Все функции должны быть каррированы;
Сделать реализацию Чёрча абстрактную по s, z;

Требования для реализации:

Функции:

0 = λsz.z
1 = λsz.sz
2 = λsz.s(sz)

create_cat = λnmc.cnm
get_name = λc.c fst
get_mur = λc.c snd
scratch = λc.create_cat (get_name c) (suc (get_mur c))

kittengarten
*/

using std::string;
using std::endl;
using std::cin;
using std::cout;
using std::function;
using std::variant;

using T = string;
using ChurchType = function<function<T(T)>(function<T(T)>)>;

template <typename F, typename G, typename T>
function<T(T)> compose(F f, G g) {
    return [=](T x) { return f(g(x)); };
}

template<typename T>
function<function<T(T)>(function<T(T)>)> ch_0 = [](function<T(T)> s) {
    return [](T z) { return z; };
};

template<typename T>
function<function<T(T)>(function<T(T)>)> ch_1 = [](function<T(T)> s) {
    return compose<function<T(T)>, function<T(T)>, T>
        (s, [](T z) { return z; });
};

template<typename T>
function<function<T(T)>(function<T(T)>)> ch_2 = [](function<T(T)> s) {
    return compose<function<T(T)>, function<T(T)>, T>(s, ch_1<T>(s));
};

template<typename T>
function<function<T(T)>(function<T(T)>)> ch_3 = [](function<T(T)> s) {
    return compose<function<T(T)>, function<T(T)>, T>(s, ch_2<T>(s));
};

T z = "z";
template<typename T>
string s(T z) {
    return "s" + z;
}

template<typename T, typename Ch>
function<function<function<function<variant<T, Ch>(T)>(Ch)>(T)>(T, T)> pair =
    [](T set_el_1, T set_el_2) { 
        return [=](T el_1){ 
            return [=](Ch el_2){
                return [=](T el) -> variant<T, Ch> {
                if (el == set_el_1){
                    return el_1;
                } else if (el == set_el_2) {
                    return el_2;
                } else {
                    throw std::invalid_argument("no such variable");
                };
            ;}
        ;}
    ;}
;};

/*
template<typename T, typename Ch>
function<function<function<variant<T, Ch>(T)>(Ch)>(T)> cat(auto pair) {
    return pair("name")("mur");
}


template<typename T, typename Ch>
function<function<variant<T, Ch>(T)>(Ch)> create_cat(auto cat, T name) {
    return cat(name);
}

template<typename T, typename Ch>
function<variant<T, Ch>(T)> create_cat(auto cat, Ch mur) {
    return cat(mur);
}
*/

template<typename T, typename Ch>
std::variant<T, Ch> get_property(auto cat0, auto var) {
    try {
        auto cat = cat0(var);
        return cat;
    } catch (const std::exception& e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        throw;
    }
}

template<typename T, typename Ch>
void print_variant(const std::variant<T, Ch>& v) {
    if (std::holds_alternative<T>(v)) {
        std::cout << "Result: " << std::get<T>(v) << std::endl;
    } else if (std::holds_alternative<ChurchType>(v)) {
        std::cout << "Function result: " << std::get<ChurchType>(v)(s<T>)(z) << std::endl;
    } else {
        std::cout << "Unexpected variant type" << std::endl;
    }
}


template<typename T, typename Ch>
void update_cat(auto cat0) {
    // Тут надо suc определять
    // и надо думать, как распаковывать по-нормальному
    return;
}

void TestChurch() {

    string res_0 = ch_0<string>(s<T>)(z);
    cout << "Result for 0: " << res_0 << endl; 

    string res_1 = ch_1<string>(s<T>)(z);
    cout << "Result for 1: " << res_1 << endl;

    string res_2 = ch_2<string>(s<T>)(z);
    cout << "Result for 2: " << res_2 << endl;

    string res_3 = ch_3<string>(s<T>)(z);
    cout << "Result for 2: " << res_3 << endl;
}

void TestKittenGarten() {
    // Проблема: я так понял, что нет возможности у C++ функций захватывать внешний скоуп, в котором они оперируют, поэтому мне 
    // необходимо передавать туда 2 параметра: объект над которым работаем и параметр действия.

    // Но если я буду лямбды писать внутри функции, то там тип только авто будет можно указывать.

    // Понятно, что можно сделать карированные функции над func(obj)(param), но я в этом особо смысла не вижу, потому что сама пара-то каррирована, а в этом и было задание.

    auto cat0 = pair<T, ChurchType>("name", "mur")("mursic")(ch_1<T>);

    auto name = get_property<string, ChurchType>(cat0, "name");
    auto mur = get_property<string, ChurchType>(cat0, "mur");
    print_variant(name);
    print_variant(mur);

}

int main() {

    TestChurch();
    TestKittenGarten();

    return 0;
}
