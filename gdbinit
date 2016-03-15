set print pretty
python
import sys
sys.path.insert(0, sorted(glob.glob('/usr/share/gcc-*/python'), reversed=True)[0])
from libstdcxx.v6.printers import register_libstdcxx_printers
register_libstdcxx_printers(gdb.current_objfile())
end
