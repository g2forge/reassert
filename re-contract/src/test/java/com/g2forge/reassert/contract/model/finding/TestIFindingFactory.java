package com.g2forge.reassert.contract.model.finding;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.ConditionFinding;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;

public class TestIFindingFactory {
	@Test
	public void test() {
		final IFindingFactory<IContractTermFinding> factory = ConditionFinding::new;
		HAssert.assertEquals(ConditionFinding.class, factory.getFindingType().getErasedType());
	}
}
