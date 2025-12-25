struct Mix {
    int x;
    int* p;
};

int main() {
    int nums[3] = {1, 2, 3};
    int* ptr = nums;
    return sizeof(int)
        + sizeof(int*)
        + sizeof(nums)
        + sizeof(struct Mix)
        + sizeof(*ptr);
}
