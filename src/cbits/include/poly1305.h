#ifndef _POLY1305_H_
#define _POLY1305_H_

int poly1305_mac(unsigned char *out,const unsigned char *in,
                 unsigned long long inlen,const unsigned char *k);
int poly1305_mac_verify(const unsigned char *h,const unsigned char *in,
                        unsigned long long inlen,const unsigned char *k);

#endif /* _POLY1305_H_ */
