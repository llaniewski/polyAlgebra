#include <stdio.h>
#include <map>
#include <set>
#include <string>
#include <cmath>

const int MAXMUL = 40;
typedef std::map<std::string,int> mon;

struct monNonZero : public mon {
    monNonZero(const mon& p) {
        for(auto i : p) if (i.second != 0) (*this)[i.first] = i.second;
    }
};

typedef std::map<monNonZero,double> pol;

void print(const pol& p) {
    for (const auto& a : p) {
        printf(" + %lf",a.second);
        for (const auto& b : a.first) {
            printf("*%s^%d", b.first.c_str(), b.second);
        }
    }
    printf("\n");
}

mon norm(const mon& p) {
    mon ret;
    for(auto i : p) if (i.second != 0) ret[i.first] = i.second;
    return ret;
}

class ToC {
    const double eps;
public:
    ToC(double eps_=1e-10) : eps(eps_) {}
    bool is_int(double val) {
        return fabs(val - round(val)) < eps;
    }

    void conv(const pol& p) {
        std::map<std::string,int> pos, neg;
        for (const auto& a : p) {
            for (const auto& b : a.first) {
                if (b.second > 0)
                    pos[b.first]++;
                else if (a.second < 0)
                    neg[b.first]++;
            }
        }
        std::map<int,int> mul, div;
        for (const auto& a : p) {
            for (int i=1; i<=MAXMUL; i++) {
                if (is_int(a.second / i)) mul[i]++;
                if (is_int(a.second * i) && (!is_int(a.second))) div[i]++;
            }
        }
        printf("pos:"); for (const auto& a : pos) printf(" %s:%d",a.first.c_str(), a.second); printf("\n");
        printf("neg:"); for (const auto& a : neg) printf(" %s:%d",a.first.c_str(), a.second); printf("\n");
        printf("mul:"); for (const auto& a : mul) printf(" %d:%d",a.first, a.second); printf("\n");
        printf("div:"); for (const auto& a : div) printf(" %d:%d",a.first, a.second); printf("\n");
    }
};


int main() {
    printf("hej\n");
    mon a;
    a["x"]++;
    pol b;
    b[norm(a)]=0.5;
    a["y"]+=2;
    b[norm(a)]=1.0/3.0;
    a["x"] = 0;
    a["y"] = 3;
    b[norm(a)]=1.0/4.0;
    print(b);
    ToC().conv(b);
    return 0;
}
