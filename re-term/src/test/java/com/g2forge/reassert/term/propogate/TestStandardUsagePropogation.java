package com.g2forge.reassert.term.propogate;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.Usage;
import com.g2forge.reassert.term.StandardUsagePropogation;
import com.g2forge.reassert.term.StandardUsageTerm;

public class TestStandardUsagePropogation {
	@Test
	public void inherits() {
		HAssert.assertNull(StandardUsagePropogation.create().apply(new Inherits(), new Usage("Usage", Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial).build())));
	}
}
