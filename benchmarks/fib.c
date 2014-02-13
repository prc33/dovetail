long fib(int n) {
	if(n < 2) {
		return n;
	} else {
		return fib(n - 1) + fib(n - 2);
	}
}

int main() {
	printf("fib(40) = %ld\n", fib(40));
	return 0;
}
