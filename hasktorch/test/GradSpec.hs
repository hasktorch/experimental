module GradSpec (spec) where

import Test.Hspec
import Control.Exception.Safe

import Torch.Tensor
import Torch.DType
import Torch.TensorFactories
import Torch.Functions
import Torch.TensorOptions
import Torch.Autograd

spec :: Spec
spec = do
  it "grad with ones" $ do
    let x = ones [] $ withRequiresGrad defaultOpts
        y = x * x + 5 * x + 3
    fmap asDouble (grad y [x]) `shouldBe` [7.0]
  it "grad with ones" $ do
    let x = ones [] $ withRequiresGrad defaultOpts
        y = ones [] $ withRequiresGrad defaultOpts
        z = x * x * y
    fmap asDouble (grad z [x]) `shouldBe` [2.0]
    fmap asDouble (grad z [y]) `shouldBe` [1.0]
