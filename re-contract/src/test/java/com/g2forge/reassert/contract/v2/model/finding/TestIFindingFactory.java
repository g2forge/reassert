package com.g2forge.reassert.contract.v2.model.finding;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.model.findings.rule.IRuleFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.ConditionFinding;

public class TestIFindingFactory {
	@Test
	public void test() {
		final IFindingFactory<IRuleFinding> factory = ConditionFinding::new;
		HAssert.assertEquals(ConditionFinding.class, factory.getFindingType().getErasedType());
	}
}
