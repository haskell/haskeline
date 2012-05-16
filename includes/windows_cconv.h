// Define the foreign import calling convention.
// On Win32, it's stdcall.
// On Win64, it's ccall.
#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

