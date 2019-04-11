terraform init -backend-config=backend.tfvars

# setup KUBECONFIG
terraform output kubeconfig > ~/.kube/auscors-eks-config
export KUBECONFIG=~/.kube/auscors-eks-config
