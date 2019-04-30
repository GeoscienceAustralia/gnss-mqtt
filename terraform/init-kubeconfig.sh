terraform init -backend-config=backend.tfvars

# setup KUBECONFIG
terraform output kubeconfig > ~/.kube/auscors-eks-config
export KUBECONFIG=~/.kube/auscors-eks-config

# assume role for kubectl
role_arn=$(terraform output eks_kubectl_role_arn)
aws sts assume-role --role-arn ${role_arn} --role-session-name auscors-eks
