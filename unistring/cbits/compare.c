#include <string.h>

int _hs__unistring__compare_offset_1(const void *s1, size_t off1,
                                     const void *s2, size_t n) {
  return memcmp((unsigned char *)s1 + off1, s2, n);
}

int _hs__unistring__compare_offset_2(const void *s1, size_t off1,
                                     const void *s2, size_t off2, size_t n) {
  return memcmp((unsigned char *)s1 + off1, (unsigned char *)s2 + off2, n);
}
