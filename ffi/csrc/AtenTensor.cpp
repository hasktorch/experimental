#include <ATen/ATen.h>
#include<MacroPatternMatch.h>

#include "AtenTensor.h"


using namespace at;




TENSOR_DEF_VIRT(Tensor)

TENSOR_DEF_NONVIRT(Tensor)
TENSOR_DEF_ACCESSOR(Tensor)

