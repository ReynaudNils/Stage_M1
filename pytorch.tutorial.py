import torch

x = torch.rand(2,2)
y = torch.rand(2,2)
z = x * y
z = torch.mul(x, y)