void memset(void *_ptr, unsigned char c, unsigned size)
{
	unsigned char *ptr = _ptr;
	while (size--)
		*ptr++ = c;
}	
