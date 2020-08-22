package com.g2forge.reassert.term.analyze.model.findings;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.report.ITerminalFinding;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class UnrecognizedTermFinding implements ITerminalFinding {
	protected final ITerm term;

	@Override
	public Level getLevel() {
		return Level.ERROR;
	}
}
