variable "cluster-name" {
  default     = "auscors"
  type        = "string"
  description = "The name of your EKS Cluster"
}

variable "workstation-external-cidr" {
  default     = "124.47.132.132/32"
  type        = "string"
  description = "The CIDR of the workstation to give access to EKS Cluster"
}

variable "aws-region" {
  default     = "ap-southeast-2"
  type        = "string"
  description = "The AWS Region to deploy EKS"
}

variable "k8s-version" {
  default     = "1.11"
  type        = "string"
  description = "Required K8s version"
}

variable "vpc-subnet-cidr" {
  default     = "10.0.0.0/16"
  type        = "string"
  description = "The VPC Subnet CIDR"
}

variable "node-instance-type" {
  default     = "t3.medium"
  type        = "string"
  description = "Worker Node EC2 instance type"
}

variable "desired-capacity" {
  default     = 2
  type        = "string"
  description = "Autoscaling Desired node capacity"
}

variable "max-size" {
  default     = 5
  type        = "string"
  description = "Autoscaling maximum node capacity"
}

variable "min-size" {
  default     = 1
  type        = "string"
  description = "Autoscaling Minimum node capacity"
}
