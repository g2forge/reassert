package com.g2forge.reassert.core.model.report;

public interface ITerminalFinding extends IFinding {
	public default ITerminalFinding getInnermostFinding() {
		return this;
	}
}
