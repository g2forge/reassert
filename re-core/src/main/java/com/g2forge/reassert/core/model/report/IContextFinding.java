package com.g2forge.reassert.core.model.report;

import org.slf4j.event.Level;

public interface IContextFinding extends IFinding {
	public IFinding getFinding();

	public default IFinding getInnermostFinding() {
		return getFinding().getInnermostFinding();
	}
	
	@Override
	public default Level getLevel() {
		return getFinding().getLevel();
	}
}
