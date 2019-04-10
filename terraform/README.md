## Key concepts:

- This code uses terraform to create an EKS Cluster and Kubernetes Worker Nodes
    - EKS is a managed Kubernetes service
    - the Kubernetes Worker Nodes are run on EC2 instances running in an Auto Scaling Group
    - the Kubernetes Worker Nodes are configured with an AWS provided AMI, with a particular version of Kubernetes
- `kubectl` command must be configured to use the EKS Cluster ($KUBECONFIG or ~/.kube/config)
- EKS Cluster essentially functions as a job scheduler. It speaks kubernetes. Given a Kubernetes yaml, the EKS cluster will task the Worker Nodes.
- to enable Worker Nodes to join the EKS cluster, they must configured with an IAM role
    - EKS doesn't provide for a native way to interface between Kubernetes and IAM, so you have an extra step (`kubectl apply -f config-map-aws-auth.yaml`)


## Terraform

```
terraform init
terraform plan
terraform apply
```

## Setup kubectl

Setup your `KUBECONFIG`

```
terraform output kubeconfig > ~/.kube/eks-config
export KUBECONFIG=~/.kube/eks-config
```

## Authorize worker nodes

Get the config from terraform output, and save it to a yaml file:

```
terraform output config-map > config-map-aws-auth.yaml
```

Apply the config map to EKS:

```
kubectl apply -f config-map-aws-auth.yaml
```

You can verify the worker nodes are joining the cluster

```
kubectl get nodes --watch
```

## Deploy Kubernetes application

Deploy the Kubernetes to the EKS cluster (to run on the Worker Nodes)

```
kubectl apply -f ../kubernetes.yaml
```

## Use application

Get load balancer external DNS for HiveMQ cluster

```
kubectl get service
```

Send messages to HiveMQ using mosquitto

```
mosquitto_sub -h <loadbalancer-external-dns>.ap-southeast-2.elb.amazonaws.com -t test &

mosquitto_pub -h <loadbalancer-external-dns>.ap-southeast-2.elb.amazonaws.com -t test -m "testing"
```
