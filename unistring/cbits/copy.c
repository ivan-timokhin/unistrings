#include <string.h>

void _hs__unistring__copy(void *dest, size_t dest_off, const void *src,
                          size_t n) {
  memcpy((unsigned char *)dest + dest_off, src, n);
}

void _hs__unistring__copy_off(void *dest, size_t dest_off, const void *src,
                              size_t src_off, size_t n) {
  memcpy((unsigned char *)dest + dest_off, (unsigned char *)src + src_off, n);
}
