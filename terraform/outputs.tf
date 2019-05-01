output "kubeconfig" {
  value = "${module.eks.kubeconfig}"
}

output "eks_kubectl_role_arn" {
  value = "${module.eks.eks_kubectl_role_arn}"
}