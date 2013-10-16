#include "util.h"
int
portable_mode ()
{
#ifdef WIN32
        if ((_access( "portable-mode", 0 )) != -1)
        {
                return 1;
        }
        else
        {
                return 0;
        }
#else
        return 0;
#endif
}
