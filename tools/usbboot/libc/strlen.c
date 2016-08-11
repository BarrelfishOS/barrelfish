int strlen(const char *s)
{
	int n = 0;
	while (*s++)
		n++;
	return n;
}
