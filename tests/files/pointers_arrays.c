int sum(int* arr, int len) {
    int total = 0;
    for (int i = 0; i < len; i = i + 1) {
        total = total + arr[i];
    }
    return total;
}

int main() {
    int nums[3] = {1, 2, 3};
    int* p = &nums[0];
    return sum(nums, 3) + *(p + 1);
}
