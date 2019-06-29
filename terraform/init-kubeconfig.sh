export AWS_ACCESS_KEY_ID
export AWS_SECRET_ACCESS_KEY

terraform init -backend-config=backend.tfvars

# setup KUBECONFIG
aws eks update-kubeconfig --name auscors

# assume role for kubectl
unset AWS_ACCESS_KEY_ID
unset AWS_SECRET_ACCESS_KEY
unset AWS_SESSION_TOKEN
role_arn=$(terraform output eks_kubectl_role_arn)
aws_credentials_json=$(aws sts assume-role --role-arn ${role_arn} --role-session-name auscors-eks)
export AWS_ACCESS_KEY_ID=$(echo "$aws_credentials_json" | jq --exit-status --raw-output .Credentials.AccessKeyId)
export AWS_SECRET_ACCESS_KEY=$(echo "$aws_credentials_json" | jq --exit-status --raw-output .Credentials.SecretAccessKey)
export AWS_SESSION_TOKEN=$(echo "$aws_credentials_json" | jq --exit-status --raw-output .Credentials.SessionToken)
