import logging

TRACE_LEVEL=4

logger = logging.getLogger("popper")

class SecondsFormatter(logging.Formatter):
    def format(self, record):
        record.elapsed_secs = record.relativeCreated / 1000.0
        return super().format(record)

# Setup once
if not logger.handlers:
    handler = logging.StreamHandler()
    formatter = SecondsFormatter("%(elapsed_secs).1fs %(message)s")
    handler.setFormatter(formatter)
    logger.addHandler(handler)
    logger.propagate = False

def set_verbosity(level):
    if level <= 1:
        logger.setLevel(logging.WARNING)
    elif level == 2:
        logger.setLevel(logging.INFO)
    elif level == 3:
        logger.setLevel(logging.DEBUG)
    elif level >= 4:
        logger.setLevel(TRACE_LEVEL)

def trace(msg, *args, **kwargs):
    if logger.isEnabledFor(TRACE_LEVEL):
        logger.log(TRACE_LEVEL, msg, *args, **kwargs)

out = logger.warning
info = logger.info
debug = logger.debug
