#define MAXN 10000
#define SQRT_MAXN 100

unsigned char primes[MAXN+1];

int main(int argc, const char *argv[])
{
    int i, j;

    for (i=2; i<=SQRT_MAXN; i++) {
        if (0 == primes[i]) {
            for (j = i*i; j<=MAXN; j+=i) {
                primes[j] = 1;
            }
        }
    }
}

