FROM python:3.7

ADD testutil.py /
ADD codeform_check.py /
ADD vulnerabilities_elliminator.py /
ADD executor.sh /
RUN chmod +x /executor.sh

ENTRYPOINT ["/executor.sh"]
