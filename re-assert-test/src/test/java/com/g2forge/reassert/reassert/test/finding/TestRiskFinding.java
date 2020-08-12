package com.g2forge.reassert.reassert.test.finding;

import org.slf4j.event.Level;

import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.term.analyze.model.findings.IRiskFinding;
import com.g2forge.reassert.term.eee.explain.model.IExplained;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TestRiskFinding implements IRiskFinding {
	protected final Level level;

	protected final String description;

	@Override
	public IExplained<TermRelation> getResult() {
		throw new UnsupportedOperationException();
	}
}
