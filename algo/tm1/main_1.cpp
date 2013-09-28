/**
 * RMQ
 */
#include <iostream>
#include <string>
#include <algorithm>
#include <limits>

using namespace std;

long long sum[2000000];

int update(int now,int L,int R,int v,int x,int y) {
    if(L<=x && y<=R) {
        sum[now]+=v;
    } else {
        int mid=(x+y)/2;
        if (L<mid)
            update(now*2,L,R,v,x,mid);
        if (mid<R)
            update(now*2+1,L,R,v,mid,y);
        long long s=sum[now*2];
        s=max(s,sum[now*2+1]);
        sum[now]+=s;
        sum[now*2]-=s;
        sum[now*2+1]-=s;
    }
    return 0;
}

long long query(int now,int L,int R,int x,int y){
    if(L<=x && y<=R)
        return sum[now];
    else {
        int mid=(x+y)/2;
        long long s=std::numeric_limits<long long>::min();
        if(L<mid)
            s=max(s,query(now*2,L,R,x,mid));
        if(mid<R)
            s=max(s,query(now*2+1,L,R,mid,y));
        return sum[now]+s;
    }
}

int main(){
    int n, q;
    int v;
    std::cin >> n >> q;
    std::string quer;
    for (int i = 0; i < n; ++i) {
        std::cin >> v;
        update(1, i, i+1, v, 0, n);
    }

    int l, r;
    for (int i = 0; i < q; ++i) {
        std::cin >> quer >> l >> r;
        --l, --r;
        if (quer == "max") {
            std::cout << query(1, l, r+1, 0, n) << std::endl;
        } else if (quer == "add") {
            std::cin >> v;
            update(1, l, r+1, v, 0, n);
        }
    }
}
