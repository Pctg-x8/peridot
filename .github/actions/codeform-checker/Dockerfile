FROM python:3.11

ADD testutil.py /
ADD codeform_check.py /
ADD vulnerabilities_elliminator.py /
ADD trailing_newline_checker.py /
ADD executor.sh /
RUN chmod +x /executor.sh

ENTRYPOINT ["/executor.sh"]
