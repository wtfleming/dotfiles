import boto.ec2
from collections import defaultdict

# requires boto (tested with 2.8.0)
# pip install boto==2.8.0

# Assumes AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY
# environment variables are set

# TODO Currently assumes everything is in us-east-1


security_groups_dict = defaultdict(list)

conn = boto.ec2.connect_to_region("us-east-1")
reservations = conn.get_all_instances()

for r in reservations:
  for i in r.instances:
    for g in i.groups:
      security_groups_dict[g.name].append(i.private_dns_name)

# Output in cssh cluster file format
for k, v in security_groups_dict.iteritems():
  print k, '=', ' '.join(v)


