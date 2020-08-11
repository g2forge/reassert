package com.g2forge.reassert.term.analyze.model.findings;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ConditionFinding implements ITerminalFinding {
	protected final IExplained<TermRelation> result;

	@Override
	public Level getLevel() {
		return isSatisfied() ? Level.INFO : Level.ERROR;
	}

	public boolean isSatisfied() {
		return getResult().get() == TermRelation.Included;
	}
}
