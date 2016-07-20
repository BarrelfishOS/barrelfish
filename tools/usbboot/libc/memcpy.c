void *memcpy(void *_dst, const void *_src, unsigned count)
{
	unsigned char *dst = _dst;
	const unsigned char *src = _src;
	while (count--)
		*dst++ = *src++;
	return _dst;
}
