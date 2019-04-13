typedef unsigned char GLubyte;
typedef void (*genericFunctionPointer)(void);
extern genericFunctionPointer OSMesaGetProcAddress(const GLubyte *);

genericFunctionPointer glXGetProcAddress(const GLubyte* name)
{
    return OSMesaGetProcAddress((const char*)name);
}

genericFunctionPointer glXGetProcAddressARB(const GLubyte* name)
{
    return OSMesaGetProcAddress((const char*)name);
}
